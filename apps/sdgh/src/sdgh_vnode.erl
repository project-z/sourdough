-module(sdgh_vnode).
-behaviour(riak_core_vnode).
-include("sdgh.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-export([
        write_event/4,
        write_data_to_disk/3
        ]).

-record(state, {write_path, partition, context, payload, num_payload=0}).

%% This is the name (as an atom) used by the supervisor when `sdgh_sup`
%% is initialized... TODO - specific as a "write" vnode master, maybe?
-define(MASTER, sdgh_vnode_master).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

write_event(Preflist, ReqID, {Bucket,Key}, Payload) ->
    riak_core_vnode_master:command(Preflist,
                                   {write, ReqID, {Bucket,Key}, Payload},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% Callbacks
init([Partition]) ->
    {ok, WritePath} = application:get_env(sdgh, worker_write_path),
    lager:warning("Well, write_path is: ~p ~n~n", [WritePath]),
    {ok, #state { write_path=WritePath, partition=Partition }}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};
handle_command({write, ReqID, {Bucket,Key}, Payload}, _Sender, State) ->
    Context = {Bucket, Key},
    NumP = State#state.num_payload + 1,
    Path = State#state.write_path,
    lager:warning("In handle_command, write_path is: ~p ~n~n", [Path]),
    %% write data to disk...
    Result = write_data_to_disk(Path, Context, Payload),
    lager:warning("I guess it didn't work, eh? ~p ~n~n", [Result]),
    S0 = State#state{context=Context, payload=Payload, num_payload=NumP},
    lager:warning("~p:handle_command:~n~n state:~n~p", [?MODULE,S0]),
    {reply, {ok, ReqID}, S0};
handle_command(Message, _Sender, State) ->
    lager:warning({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    case State#state.num_payload of
        0 -> {true, State};
        _ -> {false, State}
    end.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    lager:warning("Exiting...~n ~p~n~p~n~p~n", [_Pid, _Reason, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:warning("Terminating...~n ~p~n~p~n~n", [_Reason, _State]),
    ok.

write_data_to_disk(Path, {Bucket, Key}, Payload) ->
    P1 = Path ++ binary_to_list(Bucket) ++ "/",
    ok = filelib:ensure_dir(P1),
    File = P1 ++ binary_to_list(Key) ++ ".event",
    lager:warning("Trying to write to: ~p ~n~n Path was: ~p ~n~n", [File,P1]),
    file:write_file(File, io_lib:fwrite("~p~n", [Payload])).
