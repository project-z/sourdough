-module(sdgh).

-include("sdgh.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
        ping/1,
        get_dbg_preflist/1,
        write/3
        ]).

-define(TIMEOUT, 45000).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping(Input) ->
    DocIdx = riak_core_util:chash_key({Input, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, sdgh),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping,
                                              sdgh_vnode_master).

write(Client, ProdKey, Payload) ->
    do_write(Client, ProdKey, Payload).

%% Debugging Functions...

get_dbg_preflist({Bucket, Key}) ->
    DocIdx = riak_core_util:chash_key({Bucket,Key}),
    riak_core_apl:get_apl(DocIdx, ?N, sdgh).

%% Internal Functions

do_write(Client, ProdKey, Payload) ->
    {ok, ReqID} = sdgh_write_fsm:write(Client, ProdKey, Payload),
    wait_for_reqid(ReqID, ?TIMEOUT).

wait_for_reqid(ReqID, Timeout) ->
    lager:warning("Entered into the wait method...~n~nwaiting for: ~p~n~n",
                [ReqID]),
    receive
        {ReqID, ok} ->
            lager:warning("Received: ok from ~p", [ReqID]),
            ok;
        Msg ->
            lager:warning("Received: ~p", [Msg]),
            {ok, Msg}
    after Timeout ->
        ?PRINT(Timeout),
        {error, timeout}
    end.