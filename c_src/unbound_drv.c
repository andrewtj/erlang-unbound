#include <string.h>
#include <erl_driver.h>
#include <erl_interface.h>
#include <unbound.h>

// 4 dots for four labels. Three 63 and one 62 byte label.
// Each label-byte is a \ddd escape.
#define MAX_ASCII_NAME (4 + (63 * 4) + (62 * 4))
#define UNBOUND_DRV_RESOLVE 1
#define UNBOUND_DRV_CANCEL 2
#define UNBOUND_DRV_ADD_TA 3

typedef struct _unbound_drv_t {
    ErlDrvPort erl_port;
    struct ub_ctx *ub_ctx;
    ErlDrvEvent ub_fd;
    ErlDrvTermData term_ok;
    ErlDrvTermData term_error;
    ErlDrvTermData term_undefined;
    ErlDrvTermData term_port;
    ErlDrvTermData term_resolve;
    ErlDrvTermData term_result;
    ErlDrvTermData term_question;
    ErlDrvTermData term_response;
    ErlDrvTermData term_true;
    ErlDrvTermData term_false;
} unbound_drv_t;

/* Driver Callbacks */
static ErlDrvData start(ErlDrvPort port, char* cmd);
static void stop(ErlDrvData handle);
static ErlDrvSSizeT call(ErlDrvData drv_data,
                         unsigned int command,
                         char *buf,
                         ErlDrvSizeT len,
                         char **rbuf,
                         ErlDrvSizeT rlen,
                         unsigned int *flags);
static void ready_io(ErlDrvData handle, ErlDrvEvent ev);
static void resolve_callback(void* dd, int err, struct ub_result* result);

static ErlDrvEntry unbound_driver_entry = {
    NULL,                             /* init */
    start,                            /* startup */
    stop,                             /* shutdown */
    NULL,                             /* output */
    ready_io,                         /* ready_input */
    ready_io,                         /* ready_output */
    "unbound_drv",                    /* the name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    NULL,                             /* process */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    call,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MINOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    0,                                /* ERL_DRV_FLAGs */
    NULL,
    NULL
};

DRIVER_INIT(unbound_driver) {
  return &unbound_driver_entry;
}

static ErlDrvData start(ErlDrvPort port, char* cmd) {
    unbound_drv_t* dd = (unbound_drv_t*) driver_alloc(sizeof(unbound_drv_t));
    dd->erl_port = port;
    dd->ub_ctx = ub_ctx_create();
    if (!dd->ub_ctx) {
        driver_free(dd);
        return ERL_DRV_ERROR_GENERAL;
    }
    ub_ctx_async(dd->ub_ctx, 1);
    // TODO: ub_fd can return -1
    dd->ub_fd = (ErlDrvEvent)(long)ub_fd(dd->ub_ctx);
    driver_select(dd->erl_port, dd->ub_fd, DO_READ, 1);
    dd->erl_port = port;
    dd->term_port = driver_mk_port(port);
    dd->term_ok = driver_mk_atom("ok");
    dd->term_error = driver_mk_atom("error");
    dd->term_undefined = driver_mk_atom("undefined");
    dd->term_resolve = driver_mk_atom("resolve");
    dd->term_result = driver_mk_atom("result");
    dd->term_question = driver_mk_atom("question");
    dd->term_response = driver_mk_atom("response");
    dd->term_true = driver_mk_atom("true");
    dd->term_false = driver_mk_atom("false");
    return (ErlDrvData) dd;
}

static void stop(ErlDrvData edd) {
    unbound_drv_t* dd = (unbound_drv_t*) edd;
    driver_select(dd->erl_port, dd->ub_fd, DO_READ, 0);
    ub_ctx_delete(dd->ub_ctx);
    driver_free(dd);
}

static ErlDrvSizeT build_call_response(char **rbuf,
                                       ErlDrvSizeT rlen,
                                       int err)
{
    int out_len = 0;
    if (err == 0) {
        ei_encode_version(NULL, &out_len);
        ei_encode_atom(NULL, &out_len, "ok");
        if (rlen < out_len) {
            *rbuf = driver_alloc(out_len);
        }
        out_len = 0;
        ei_encode_version(*rbuf, &out_len);
        ei_encode_atom(*rbuf, &out_len, "ok");
    } else {
        const char *strerr = ub_strerror(err);
        int strerr_len = strerr ? strlen(strerr) : 0;
        ei_encode_version(NULL, &out_len);
        ei_encode_tuple_header(NULL, &out_len, 2);
        ei_encode_atom(NULL, &out_len, "error");
        ei_encode_tuple_header(NULL, &out_len, 2);
        ei_encode_long(NULL, &out_len, err);
        ei_encode_binary(NULL, &out_len, strerr, strerr_len);
        if (rlen < out_len) {
            *rbuf = driver_alloc(out_len);
        }
        out_len = 0;
        ei_encode_version(*rbuf, &out_len);
        ei_encode_tuple_header(*rbuf, &out_len, 2);
        ei_encode_atom(*rbuf, &out_len, "error");
        ei_encode_tuple_header(*rbuf, &out_len, 2);
        ei_encode_long(*rbuf, &out_len, err);
        ei_encode_binary(*rbuf, &out_len, strerr, strerr_len);
    }
    return out_len;
}

static ErlDrvSSizeT call(ErlDrvData edd,
                         unsigned int cmd,
                         char *buf,
                         ErlDrvSizeT len,
                         char **rbuf,
                         ErlDrvSizeT rlen,
                         unsigned int *flags)
{
    int version, index, rindex, out_len, async_id, cl, ty;
    ei_term term;
    unbound_drv_t* dd = (unbound_drv_t*) edd;
    char name[MAX_ASCII_NAME+1];
    long name_size = 0;

    index = 0;

    ei_decode_version(buf, &index, &version);
    ei_decode_ei_term(buf, &index, &term);
    
    if (cmd == UNBOUND_DRV_RESOLVE) {
        if (term.ei_type != ERL_SMALL_TUPLE_EXT || term.arity != 3) {
            goto badarg;
        }
        ei_decode_ei_term(buf, &index, &term);
        if (term.ei_type != ERL_BINARY_EXT || term.size > MAX_ASCII_NAME) {
            goto badarg;
        }
        ei_decode_binary(buf, &index, name, &name_size);
        name[name_size] = 0;
        ei_decode_ei_term(buf, &index, &term);
        if (term.ei_type != ERL_SMALL_INTEGER_EXT) {
            goto badarg;
        }
        ty = term.value.i_val;
        ei_decode_ei_term(buf, &index, &term);
        if (term.ei_type != ERL_SMALL_INTEGER_EXT) {
            goto badarg;
        }
        cl = term.value.i_val;
        // TODO: check return values
        ub_resolve_async(dd->ub_ctx, name, ty, cl, dd, resolve_callback, &async_id);
        out_len = 0;
        ei_encode_version(NULL, &out_len);
        ei_encode_tuple_header(NULL, &out_len, 2);
        ei_encode_atom(NULL, &out_len, "ok");
        ei_encode_long(NULL, &out_len, async_id);
        if(rlen < out_len) {
            *rbuf = driver_alloc(out_len);
        }
        rindex = 0;
        ei_encode_version(*rbuf, &rindex);
        ei_encode_tuple_header(*rbuf, &rindex, 2);
        ei_encode_atom(*rbuf, &rindex, "ok");
        ei_encode_long(*rbuf, &rindex, async_id);
        return out_len;
    } else if (cmd == UNBOUND_DRV_CANCEL) {
        if (term.ei_type != ERL_SMALL_INTEGER_EXT) {
            goto badarg;
        }
        int err = ub_cancel(dd->ub_ctx, term.value.i_val);
        return build_call_response(rbuf, rlen, err);
    } else if (cmd == UNBOUND_DRV_ADD_TA) {
        if (term.ei_type != ERL_BINARY_EXT) {
            goto badarg;
        }
        char *ta = driver_alloc(1 + term.size);
        long ta_size;
        ei_decode_binary(buf, &index, ta, &ta_size);
        ta[ta_size] = 0;
        int err = ub_ctx_add_ta(dd->ub_ctx, ta);
        driver_free(ta);
        return build_call_response(rbuf, rlen, err);
    }
    badarg:
    return -1;
}

static void ready_io(ErlDrvData edd, ErlDrvEvent ev)
{
    unbound_drv_t* dd = (unbound_drv_t*) edd;
    ub_process(dd->ub_ctx);
}

static void out_term_push(ErlDrvTermData *out, int *index, ErlDrvTermData term)
{
    if (out) {
        out[*index] = term;
    }
    (*index)++;
}

static void out_term_push2(ErlDrvTermData *out, int *index, ErlDrvTermData term1, ErlDrvTermData term2)
{
    out_term_push(out, index, term1);
    out_term_push(out, index, term2);
}

static void out_term_push3(ErlDrvTermData *out, int *index, ErlDrvTermData term1, ErlDrvTermData term2, ErlDrvTermData term3)
{
    out_term_push(out, index, term1);
    out_term_push(out, index, term2);
    out_term_push(out, index, term3);
}

static void out_term_push_str(ErlDrvTermData *out, int *index, const char* str)
{
    int len = str ? strlen(str) : 0;
    out_term_push3(out, index, ERL_DRV_BUF2BINARY, (ErlDrvTermData)str, len);
}

static void build_result(unbound_drv_t *dd, int err, struct ub_result* result, ErlDrvTermData *out, int *index)
{
    out_term_push2(out, index, ERL_DRV_PORT, dd->term_port);
    // TODO: should be 'resolve_callback'
    out_term_push2(out, index, ERL_DRV_ATOM, dd->term_resolve);
    if (err == 0) {
        out_term_push2(out, index, ERL_DRV_ATOM, dd->term_undefined);
    } else {
        out_term_push2(out, index, ERL_DRV_INT, err);
        out_term_push_str(out, index, ub_strerror(err));
        out_term_push2(out, index, ERL_DRV_TUPLE, 2);
    }
    if (result) {
        out_term_push2(out, index, ERL_DRV_ATOM, dd->term_result);
        out_term_push2(out, index, ERL_DRV_ATOM, dd->term_question);
        out_term_push_str(out, index, result->qname);
        out_term_push2(out, index, ERL_DRV_INT, result->qtype);
        out_term_push2(out, index, ERL_DRV_INT, result->qclass);
        out_term_push2(out, index, ERL_DRV_TUPLE, 4);
        int ancount = 0;
        for (; result->data[ancount]; ancount++) {
            out_term_push3(out, index, ERL_DRV_BUF2BINARY, (ErlDrvTermData)result->data[ancount], result->len[ancount]);
        }
        out_term_push(out, index, ERL_DRV_NIL);
        out_term_push2(out, index, ERL_DRV_LIST, ancount + 1);
        out_term_push_str(out, index, result->canonname);
        out_term_push2(out, index, ERL_DRV_INT, result->rcode);
        out_term_push3(out, index, ERL_DRV_BUF2BINARY, (ErlDrvTermData)result->answer_packet, result->answer_len);
        out_term_push2(out, index, ERL_DRV_ATOM, result->havedata ? dd->term_true : dd->term_false);
        out_term_push2(out, index, ERL_DRV_ATOM, result->nxdomain ? dd->term_true : dd->term_false);
        out_term_push2(out, index, ERL_DRV_ATOM, result->secure ? dd->term_true : dd->term_false);
        out_term_push2(out, index, ERL_DRV_ATOM, result->bogus ? dd->term_true : dd->term_false);
        out_term_push_str(out, index, result->why_bogus);
        out_term_push2(out, index, ERL_DRV_INT, result->ttl);
        out_term_push2(out, index, ERL_DRV_TUPLE, 12);
    } else {
        out_term_push2(out, index, ERL_DRV_ATOM, dd->term_undefined);
    }
    out_term_push2(out, index, ERL_DRV_TUPLE, 3);
    out_term_push2(out, index, ERL_DRV_TUPLE, 2);
}

static void resolve_callback(void* edd, int err, struct ub_result* result)
{
    unbound_drv_t* dd = (unbound_drv_t*) edd;
    int index = 0;
    build_result(dd, err, result, NULL, &index);
    ErlDrvTermData *out = driver_alloc(index * sizeof(ErlDrvTermData));
    index = 0;
    build_result(dd, err, result, out, &index);
    erl_drv_output_term(dd->term_port, out, index);
    driver_free(out);
    ub_resolve_free(result);
}
