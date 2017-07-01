-module(unbound_drv).
-export([load/0]).

-define(DRIVER_NAME, ?MODULE_STRING).

load() ->
    {ok, Drivers} = erl_ddll:loaded_drivers(),
    case lists:member(?DRIVER_NAME, Drivers) of
    true -> ok;
    false ->
        case erl_ddll:load(priv_dir(), ?DRIVER_NAME) of
        ok -> ok;
        {error, Error} ->
            error_logger:error_msg(
              ?MODULE_STRING ": Error loading ~p: ~p~n",
              [?DRIVER_NAME, erl_ddll:format_error(Error)]
             ),
            {error, Error}
        end
    end.
    
priv_dir() ->
    case code:priv_dir(unbound) of
        List when is_list(List) -> List;
        {error, bad_name} ->
            filename:join(filename:dirname(code:which(?MODULE)), "../priv")
    end.