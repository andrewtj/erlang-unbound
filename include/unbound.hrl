-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(ub_question, {name, type, class}).

-record(ub_result, {
    question :: #ub_question{},
    data,
    canonname,
    rcode,
    answer_packet,
    havedata,
    nxdomain,
    secure,
    bogus,
    why_bogus,
    ttl
}).

-record(ub_callback, {port, error, result :: #ub_result{}}).
