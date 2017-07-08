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
#define UNBOUND_DRV_ADD_TA_AUTR 4
#define UNBOUND_DRV_ADD_TA_FILE 5
#define UNBOUND_DRV_HOSTS 6
#define UNBOUND_DRV_RESOLVCONF 7
#define UNBOUND_DRV_SET_FWD 8

typedef struct _request_t request_t;

typedef struct _unbound_drv_t {
    ErlDrvPort erl_port;
    struct ub_ctx *ub_ctx;
    ErlDrvEvent ub_fd;
    ErlDrvTermData term_error;
    ErlDrvTermData term_port;
    ErlDrvTermData term_result;
    ErlDrvTermData term_question;
    ErlDrvTermData term_callback;
    ErlDrvTermData term_true;
    ErlDrvTermData term_false;
    ErlDrvTermData term_ub;
    ErlDrvTermData term_nomem;
    request_t **requests;
    int requests_len;
    int requests_cap;
} unbound_drv_t;

struct _request_t {
    int id;
    unbound_drv_t *dd;
};

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
    if (!dd) {
        errno = ENOMEM;
        return ERL_DRV_ERROR_ERRNO;
    }
    dd->requests = driver_alloc(sizeof(request_t));
    if (!dd->requests) {
        driver_free(dd);
        errno = ENOMEM;
        return ERL_DRV_ERROR_ERRNO;
    }
    dd->requests_len = 0;
    dd->requests_cap = 1;
    dd->ub_ctx = ub_ctx_create();
    if (!dd->ub_ctx) {
        driver_free(dd->requests);
        driver_free(dd);
        errno = ENOMEM;
        return ERL_DRV_ERROR_ERRNO;
    }
    int err = ub_ctx_async(dd->ub_ctx, 1);
    if (err != 0) {
        ub_ctx_delete(dd->ub_ctx);
        driver_free(dd->requests);
        driver_free(dd);
        return ERL_DRV_ERROR_GENERAL;
    }
    int fd = ub_fd(dd->ub_ctx);
    if (fd == -1) {
        ub_ctx_delete(dd->ub_ctx);
        driver_free(dd->requests);
        driver_free(dd);
        errno = EBADF;
        return ERL_DRV_ERROR_ERRNO;
    }
    dd->erl_port = port;
    dd->ub_fd = (ErlDrvEvent)(long)fd;
    dd->term_port = driver_mk_port(port);
    dd->term_error = driver_mk_atom("error");
    dd->term_result = driver_mk_atom("ub_result");
    dd->term_question = driver_mk_atom("ub_question");
    dd->term_callback = driver_mk_atom("ub_drv_callback");
    dd->term_true = driver_mk_atom("true");
    dd->term_false = driver_mk_atom("false");
    dd->term_ub = driver_mk_atom("ub");
    dd->term_nomem = driver_mk_atom("nomem");
    // always succeeds unless ready_input/ready_output is NULL
    driver_select(dd->erl_port, dd->ub_fd, DO_READ, 1);
    return (ErlDrvData) dd;
}

static void stop(ErlDrvData edd) {
    unbound_drv_t* dd = (unbound_drv_t*) edd;
    driver_select(dd->erl_port, dd->ub_fd, DO_READ, 0);
    for (int i = 0; i < dd->requests_len; i++) {
        request_t *r = dd->requests[i];
        ub_cancel(dd->ub_ctx, r->id);
        driver_free(r);
    }
    ub_ctx_delete(dd->ub_ctx);
    driver_free(dd->requests);
    driver_free(dd);
}

static int build_call_response_nomem(char **rbuf, ErlDrvSizeT rlen)
{
    int out_len = 0;
    ei_encode_version(NULL, &out_len);
    ei_encode_tuple_header(NULL, &out_len, 2);
    ei_encode_atom(NULL, &out_len, "error");
    ei_encode_atom(NULL, &out_len, "nomem");
    if (rlen < out_len) {
        *rbuf = driver_alloc(out_len);
        if (!rbuf) {
            return -1;
        }
    }
    out_len = 0;
    ei_encode_version(*rbuf, &out_len);
    ei_encode_tuple_header(*rbuf, &out_len, 2);
    ei_encode_atom(*rbuf, &out_len, "error");
    ei_encode_atom(*rbuf, &out_len, "nomem");
    return out_len;
}

static void build_call_response(char *rbuf, int *out_len, int err)
{
    *out_len = 0;
    ei_encode_version(rbuf, out_len);
    if (err == 0) {
        ei_encode_atom(rbuf, out_len, "ok");
    } else {
        const char *strerr = ub_strerror(err);
        int strerr_len = strerr ? strlen(strerr) : 0;
        ei_encode_tuple_header(rbuf, out_len, 2);
        ei_encode_atom(rbuf, out_len, "error");
        ei_encode_tuple_header(rbuf, out_len, 3);
        ei_encode_atom(rbuf, out_len, "ub");
        ei_encode_long(rbuf, out_len, err);
        ei_encode_binary(rbuf, out_len, strerr, strerr_len);
    }
}

static ErlDrvSSizeT call(ErlDrvData edd,
                         unsigned int cmd,
                         char *buf,
                         ErlDrvSizeT len,
                         char **rbuf,
                         ErlDrvSizeT rlen,
                         unsigned int *flags)
{
    int err = 0, version, index = 0, rindex, out_len, cl, ty;
    ei_term term;
    unbound_drv_t* dd = (unbound_drv_t*) edd;
    char name[MAX_ASCII_NAME+1];
    long name_size = 0;
    char *arg = NULL;
    long arg_size = 0;

    ei_decode_version(buf, &index, &version);
    ei_decode_ei_term(buf, &index, &term);
    
    switch (cmd) {
    case UNBOUND_DRV_RESOLVE:
        if (term.ei_type != ERL_SMALL_TUPLE_EXT || term.arity != 3) {
            return -1;
        }
        ei_decode_ei_term(buf, &index, &term);
        if (term.ei_type != ERL_BINARY_EXT || term.size > MAX_ASCII_NAME) {
            return -1;
        }
        ei_decode_binary(buf, &index, name, &name_size);
        name[name_size] = 0;
        ei_decode_ei_term(buf, &index, &term);
        if (term.ei_type != ERL_SMALL_INTEGER_EXT) {
            return -1;
        }
        ty = term.value.i_val;
        ei_decode_ei_term(buf, &index, &term);
        if (term.ei_type != ERL_SMALL_INTEGER_EXT) {
            return -1;
        }
        cl = term.value.i_val;
        if (dd->requests_len == dd->requests_cap) {
            ErlDrvSizeT new_cap = dd->requests_cap * 2;
            request_t ** new_requests = driver_realloc(dd->requests, new_cap * sizeof(request_t));
            if (!new_requests) {
                return build_call_response_nomem(rbuf, rlen);
            }
            dd->requests = new_requests;
            dd->requests_cap = new_cap;
        }
        request_t * new_req = driver_alloc(sizeof(request_t));
        if (!new_req) {
            return build_call_response_nomem(rbuf, rlen);
        }
        new_req->dd = dd;
        ub_resolve_async(dd->ub_ctx, name, ty, cl, new_req, resolve_callback, &new_req->id);
        dd->requests[dd->requests_len++] = new_req;
        out_len = 0;
        ei_encode_version(NULL, &out_len);
        ei_encode_tuple_header(NULL, &out_len, 2);
        ei_encode_atom(NULL, &out_len, "ok");
        ei_encode_long(NULL, &out_len, new_req->id);
        if(rlen < out_len) {
            char *new_rbuf = driver_alloc(out_len);
            if (!new_rbuf) {
                return build_call_response_nomem(rbuf, rlen);
            }
            *rbuf = new_rbuf;
        }
        rindex = 0;
        ei_encode_version(*rbuf, &rindex);
        ei_encode_tuple_header(*rbuf, &rindex, 2);
        ei_encode_atom(*rbuf, &rindex, "ok");
        ei_encode_long(*rbuf, &rindex, new_req->id);
        return out_len;
    case UNBOUND_DRV_CANCEL:
        if (term.ei_type != ERL_SMALL_INTEGER_EXT) {
            return -1;
        }
        for (int i = 0; i < dd->requests_len; i++) {
            if (dd->requests[i]->id == term.value.i_val) {
                request_t *r = dd->requests[i];
                err = ub_cancel(dd->ub_ctx, r->id);
                dd->requests[i] = dd->requests[--dd->requests_len];
                driver_free(r);
                goto cancel_respond;
            }
        }
        cancel_respond:
        build_call_response(NULL, &out_len, err);
        if (rlen < out_len) {
            char * new_rbuf = driver_alloc(out_len);
            if (!new_rbuf) {
                return build_call_response_nomem(rbuf, rlen);
            }
            *rbuf = new_rbuf;
        }
        build_call_response(*rbuf, &out_len, err);
        return out_len;
    case UNBOUND_DRV_ADD_TA:
    case UNBOUND_DRV_ADD_TA_AUTR:
    case UNBOUND_DRV_ADD_TA_FILE:
    case UNBOUND_DRV_SET_FWD:
        if (term.ei_type != ERL_BINARY_EXT) {
            return -1;
        }
        arg = driver_alloc(1 + term.size);
        if (!arg) {
            return build_call_response_nomem(rbuf, rlen);
        }
        ei_decode_binary(buf, &index, arg, &arg_size);
        arg[arg_size] = 0;
        if (cmd == UNBOUND_DRV_ADD_TA) {
            err = ub_ctx_add_ta(dd->ub_ctx, arg);
        } else if (cmd == UNBOUND_DRV_ADD_TA_AUTR) {
            err = ub_ctx_add_ta_autr(dd->ub_ctx, arg);
        } else if (cmd == UNBOUND_DRV_ADD_TA_FILE) {
            err = ub_ctx_add_ta_file(dd->ub_ctx, arg);
        } else { // cmd == UNBOUND_DRV_SET_FWD
            err = ub_ctx_set_fwd(dd->ub_ctx, arg);
        }
        if (arg) {
            driver_free(arg);
        }
        build_call_response(NULL, &out_len, err);
        if (rlen < out_len) {
            char * new_rbuf = driver_alloc(out_len);
            if (!new_rbuf) {
                return build_call_response_nomem(rbuf, rlen);
            }
            *rbuf = new_rbuf;
        }
        build_call_response(*rbuf, &out_len, err);
        return out_len;
    case UNBOUND_DRV_RESOLVCONF:
    case UNBOUND_DRV_HOSTS:
        if (term.ei_type != ERL_BINARY_EXT) {
            return -1;
        }
        if (term.size > 0) {
            arg = driver_alloc(1 + term.size);
            if (!arg) {
                return build_call_response_nomem(rbuf, rlen);
            }
            ei_decode_binary(buf, &index, arg, &arg_size);
            arg[arg_size] = 0;
        }
        if (cmd == UNBOUND_DRV_RESOLVCONF) {
            err = ub_ctx_resolvconf(dd->ub_ctx, arg);
        } else { // cmd == UNBOUND_DRV_HOSTS
            err = ub_ctx_hosts(dd->ub_ctx, arg);
        }
        driver_free(arg);
        build_call_response(NULL, &out_len, err);
        if (rlen < out_len) {
            char * new_rbuf = driver_alloc(out_len);
            if (!new_rbuf) {
                return build_call_response_nomem(rbuf, rlen);
            }
            *rbuf = new_rbuf;
        }
        build_call_response(*rbuf, &out_len, err);
        return out_len;
    }
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

static void build_result(unbound_drv_t *dd, int id, int err, struct ub_result* result, ErlDrvTermData *out, int *index)
{
    out_term_push2(out, index, ERL_DRV_ATOM, dd->term_callback);
    out_term_push2(out, index, ERL_DRV_PORT, dd->term_port);
    out_term_push2(out, index, ERL_DRV_INT, id);
    if (err == 0) {
        out_term_push2(out, index, ERL_DRV_ATOM, dd->term_false);
    } else {
        out_term_push2(out, index, ERL_DRV_ATOM, dd->term_error);
        out_term_push2(out, index, ERL_DRV_ATOM, dd->term_ub);
        out_term_push2(out, index, ERL_DRV_INT, err);
        out_term_push_str(out, index, ub_strerror(err));
        out_term_push2(out, index, ERL_DRV_TUPLE, 3);
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
        out_term_push2(out, index, ERL_DRV_ATOM, dd->term_false);
    }
    out_term_push2(out, index, ERL_DRV_TUPLE, 5);
}

static void resolve_callback(void* arg, int err, struct ub_result* result)
{
    request_t* r = (request_t*) arg;
    unbound_drv_t* dd = r->dd;
    int index = 0;
    build_result(dd, r->id, err, result, NULL, &index);
    ErlDrvTermData *out = driver_alloc(index * sizeof(ErlDrvTermData));
    if (out) {
        index = 0;
        build_result(dd, r->id, err, result, out, &index);
        erl_drv_output_term(dd->term_port, out, index);
        driver_free(out);
    } else {
        ErlDrvTermData out[] = {ERL_DRV_ATOM, dd->term_callback,
                                ERL_DRV_PORT, dd->term_port,
                                ERL_DRV_INT, r->id,
                                ERL_DRV_ATOM, dd->term_error,
                                ERL_DRV_ATOM, dd->term_nomem,
                                ERL_DRV_TUPLE, 2,
                                ERL_DRV_ATOM, dd->term_false,
                                ERL_DRV_TUPLE, 5};
        erl_drv_output_term(dd->term_port, out, sizeof(out)/sizeof(out[0]));
    }
    ub_resolve_free(result);
    for (int i = 0; i < dd->requests_len; i++) {
        if (dd->requests[i] == r) {
            dd->requests[i] = dd->requests[--dd->requests_len];
            driver_free(r);
            return;
        }
    }
}
