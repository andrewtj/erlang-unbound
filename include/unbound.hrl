-record(ub_question, {name, type, class}).

-record(ub_result, {
    question :: #ub_question{},
    data :: [binary()],
    canonname :: binary(),
    rcode :: integer(),
    answer_packet :: binary(),
    havedata :: boolean(),
    nxdomain :: boolean(),
    secure :: boolean(),
    bogus :: boolean(),
    why_bogus :: binary(),
    ttl :: integer()
}).

-record(ub_callback, {
    process :: pid(),
    ref :: unbound_server:async_ref(),
    error :: false | {error, nomem} | {error, {ub, integer(), binary()}},
    result :: #ub_result{} | false
}).

-record(ub_drv_callback, {
    port :: port(),
    id :: integer(),
    error :: false| {error, nomem} | {error, {ub, integer(), binary()}},
    result :: #ub_result{} | false
}).
