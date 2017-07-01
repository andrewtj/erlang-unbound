#include <string.h>
#include <erl_driver.h>
#include <erl_interface.h>
#include <unbound.h>

// 4 dots for four labels. Three 63 and one 62 byte label.
// Each label-byte is a \ddd escape.
#define MAX_ASCII_NAME (4 + (63 * 4) + (62 * 4))
#define UNBOUND_DRV_RESOLVE 1

typedef struct _unbound_drv_t {
    ErlDrvPort erl_port;
    struct ub_ctx *ub_ctx;
    ErlDrvEvent ub_fd;
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
    dd->ub_fd = (ErlDrvEvent)(long)ub_fd(dd->ub_ctx);
    driver_select(dd->erl_port, dd->ub_fd, DO_READ, 1);
    dd->erl_port = port;
    dd->term_port = driver_mk_port(port);
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
    }
    badarg:
    return -1;
}

static void ready_io(ErlDrvData edd, ErlDrvEvent ev)
{
    unbound_drv_t* dd = (unbound_drv_t*) edd;
    ub_process(dd->ub_ctx);
}

static void resolve_callback(void* edd, int err, struct ub_result* result)
{
    unbound_drv_t* dd = (unbound_drv_t*) edd;    

    if (!result) {
        // can't trace it back to a question, so give up.
        // TODO: send err?
        return;
    }

    int ancount = 0;
    while (result->data[ancount]) { ancount++; }
    
    ErlDrvTermData before_an[] = {
        ERL_DRV_PORT, dd->term_port,
                ERL_DRV_ATOM, dd->term_result,
                ERL_DRV_INT, err,
                    ERL_DRV_ATOM, dd->term_question,
                    ERL_DRV_BUF2BINARY, (ErlDrvTermData) result->qname, strlen(result->qname),
                    ERL_DRV_INT, result->qtype,
                    ERL_DRV_INT, result->qclass,
                ERL_DRV_TUPLE, 4,
                    ERL_DRV_ATOM, dd->term_response,
                    ERL_DRV_INT, result->rcode,
                    ERL_DRV_INT, result->ttl,
                    ERL_DRV_ATOM, result->nxdomain ? dd->term_true : dd->term_false,
                    ERL_DRV_ATOM, result->secure ? dd->term_true : dd->term_false,
                    ERL_DRV_ATOM, result->bogus ? dd->term_true : dd->term_false,
                    ERL_DRV_BUF2BINARY, (ErlDrvTermData) result->why_bogus, result->why_bogus ? strlen(result->why_bogus) : 0,
                    ERL_DRV_BUF2BINARY, (ErlDrvTermData) result->answer_packet, result->answer_len,
    };
    ErlDrvTermData after_an[] = {
                ERL_DRV_TUPLE, 9,
            ERL_DRV_TUPLE, 4,
        ERL_DRV_TUPLE, 2
    };

    int before_an_size = sizeof(before_an);
    int ansize = ((ancount * 3) + 3) * sizeof(ErlDrvTermData);
    int after_an_size = sizeof(after_an);
    
    ErlDrvTermData *spec = driver_alloc(before_an_size + ansize + after_an_size);
    int spec_i = 0;
    for (int max = before_an_size / sizeof(before_an[0]); spec_i < max; spec_i++)
    {
        spec[spec_i] = before_an[spec_i];
    }

    int an_i = 0;
    while (result->data[an_i]) {
        spec[spec_i++] = ERL_DRV_BUF2BINARY;
        spec[spec_i++] = (ErlDrvTermData)result->data[an_i];
        spec[spec_i++] = result->len[an_i];
        an_i++;
    }
    spec[spec_i++] = ERL_DRV_NIL;
    spec[spec_i++] = ERL_DRV_LIST;
    spec[spec_i++] = an_i + 1;
    for (int i = 0; i < after_an_size/sizeof(after_an[0]); i++) {
        spec[spec_i++] = after_an[i];
    }
    erl_drv_output_term(dd->term_port, spec, spec_i);
    driver_free(spec);

    ub_resolve_free(result);
}
