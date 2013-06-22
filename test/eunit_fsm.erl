-module(eunit_fsm).

-export([
    translate_cmd/2,
    get/2
  ]).
-define(Expr(X),??X).

translate_cmd(Id, {state, is, X}) ->
    case get(Id, "StateName") of
        X -> true;
        _V ->  .erlang:error({statename_match_failed,
                              [{module, ?MODULE},
                               {line, ?LINE},
                               {expected, X},
                               {value, _V}]})
    end;
translate_cmd(_Id, {call, M, F, A, X}) ->
    case apply(M, F, A) of
        X -> ok;
        _V ->  .erlang:error({function_call_match_failed,
                              [{module, ?MODULE},
                               {line, ?LINE},
                               {expression, ?Expr(apply(M,F,A))},
                               {expected, X},
                               {value, _V}]})
    end;
translate_cmd(Id, {loopdata, is, X}) ->
    case tl(tuple_to_list(get(Id," StateData"))) of
        X    -> true;
        _V ->    .erlang:error({loopdata_match_failed,
                                [{module, ?MODULE},
                                 {line, ?LINE},
                                 {expected, X},
                                 {value, _V}]})
    end.

% StateName or StateData
get(Id, Which) ->
    {status, _Pid, _ModTpl, List} = sys:get_status(Id),
    AllData = lists:flatten([ X || {data, X} <- lists:last(List) ]),
    proplists:get_value(Which, AllData).