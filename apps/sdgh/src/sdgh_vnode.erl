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
-define(DEFAULT_PATH, "sdgh").

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
    Path = case application:get_env(sdgh, worker_write_path) of
                {ok, WritePath} -> WritePath;
                _Any -> ?DEFAULT_PATH
            end,
    {ok, #state{ write_path=Path, partition=Partition }}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};
handle_command({write, ReqID, {Bucket,Key}, Payload}, _Sender, State) ->
    Context = {Bucket, Key},
    NumP = State#state.num_payload + 1,
    Path = State#state.write_path,
    lager:warning("In handle_command, write_path is: ~p ~n~n", [Path]),
    %% write data to disk...
    ok = write_data_to_disk(Path, Context, Payload),
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
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

build_path(BasePath, Folder, Suffix) ->
    case is_binary(Folder) of
            true -> lists:concat([BasePath, binary_to_list(Folder), Suffix]);
            false -> BasePath ++ Folder ++ Suffix
    end.

write_data_to_disk(Path, {Bucket, Key}, Payload) ->
    P1 = build_path(Path, Bucket, "/"),
    File = build_path(P1, Key, ".event"),
    ok = filelib:ensure_dir(P1),

    lager:warning("Trying to write to: ~p ~n~n Path was: ~p ~n~n", [File,P1]),
    file:write_file(File, Payload).

%%===================================================================
%% Test code
%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

verify_init_test() ->
    {ok, State} = init([9990090909]),
    ?DEFAULT_PATH = State#state.write_path,
    9990090909 = State#state.partition.

build_path_reg_string_test() ->
    "/tmp/qux/" = build_path("/tmp/", "qux", "/"),
    "/tmp/qux/goo.ndy" = build_path(build_path("/tmp/", "qux", "/"),
        "goo", ".ndy").

write_data_to_disk_bitdata_test() ->
    os:cmd("rm -rf /tmp/qux"),
    P1 = "/tmp/qux/",
    Bucket = <<"Cobain">>,
    Key = <<"chim_chim">>,
    Payload = <<"I'm on a plain, I can't complain; Somewhere I've heard this before">>,
    ok = write_data_to_disk(P1, {Bucket, Key}, Payload),
    Fullname = build_path(build_path(P1, Bucket, "/"), Key, ".event"),
    BinContents = list_to_binary(os:cmd("cat " ++ Fullname)),
    BinContents =:= Payload.

write_data_to_disk_reg_data_test() ->
    os:cmd("rm -rf /tmp/qux"),
    P1 = "/tmp/qux/",
    Bucket = "Cobain",
    Key = "chim_chim",
    Payload = "I'm on a plain, I can't complain; Somewhere I've heard this before",
    ok = write_data_to_disk(P1, {Bucket, Key}, Payload),
    Fullname = build_path(build_path(P1, Bucket, "/"), Key, ".event"),
    Payload =:= os:cmd("cat " ++ Fullname).

%% handle_command({write, ReqID, {Bucket,Key}, Payload}, _Sender, State) ->
handle_command_write_test() ->
    os:cmd("rm -rf /tmp/qux"),
    ReqID = 114811747,
    P1 = "/tmp/qux/",
    Bucket = <<"Cobain">>,
    Key = <<"chim_chim">>,
    Payload = <<"I'm on a plain, I can't complain; Somewhere I've heard this before">>,
    %% get a State data record from init/1
    {ok, State} = init([90990909650]),
    %% jam a different, local path into the StateData
    S0 = State#state{ write_path=P1 },
    NumP = State#state.num_payload,
    Resp = handle_command({write, ReqID, {Bucket,Key}, Payload}, "eunit", S0),
    {reply, {ok, ReqID}, S1} = Resp,
    Payload = S1#state.payload,
    ?assert(NumP < S1#state.num_payload),
    Fullname = build_path(build_path(P1, Bucket, "/"), Key, ".event"),
    Payload =:= os:cmd("cat " ++ Fullname).

-endif.