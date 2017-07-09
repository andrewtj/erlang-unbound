#!/usr/bin/env escript

main(Args) ->
    Out = case Args of
        [] -> standard_io;
        [Target|_] ->
            {ok, OutFile} = file:open(Target, [write]),
            OutFile
    end,
    %% dns-parameters-4.csv sourced from:
    %% https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml
    ScriptDir = filename:dirname(escript:script_name()),
    CsvFile = filename:join(ScriptDir, "dns-parameters-4.csv"),
    {ok, In} = file:open(CsvFile, [read]),
    ok = io:fwrite(Out,
"-ifndef(UNBOUND_PARAMS_HRL).
-define(UNBOUND_PARAMS_HRL, true).
-define(UB_CL_IN, 1).
-define(UB_CL_CH, 3).
-define(UB_CL_HS, 4).
", []),
    ok = scrape(In, Out),
    ok = io:fwrite(Out, "-endif.~n", []).

scrape(In, Out) ->
    case file:read_line(In) of
        eof -> ok;
        {ok, Data} ->
            [MaybeTy, MaybeVal|_] = string:split(Data, ",", all),
            case {parse_ty(MaybeTy), parse_val(MaybeVal)} of
                {A, B} when A =:= false orelse B =:= false -> scrape(In, Out);
                {Ty, Val} ->
                    io:fwrite(Out, "-define(UB_TY_~s, ~s).~n", [Ty, Val]),
                    scrape(In, Out)
            end;
        Other -> Other
    end.

parse_ty("*") -> "ANY";
parse_ty(Other) ->
    case lists:all(fun(T) -> T >= $A andalso T =< $Z end, Other) of
        true -> Other;
        false -> false
    end.

parse_val(S) ->
    case lists:all(fun(T) -> T >= $0 andalso T =< $9 end, S) of
        true -> S;
        false -> false
    end.
