%% @doc The coordinator for writing & indexing events.
-module(sdgh_write_fsm).
-behaviour(gen_fsm).
-include("sdgh.hrl").

%% API
-export([
    start_link/5,
    write/3
    ]).

%% Callbacks
-export([
    init/1,
    code_change/4,
    handle_event/3,
    handle_info/3,
    handle_sync_event/4,
    terminate/3
    ]).

%% States
-export([
    prepare/2,
    execute/2,
    waiting/2]).

-record(state, {
                req_id :: pos_integer(),
                from :: pid(),
                client :: string(),
                context,
                payload,
                preflist :: riak_core_apl:preflist2(),
                num_w = 0 :: non_neg_integer()
}).

-define(TIMEOUT, 8000).

%% API

start_link(ReqID, From, Client, {Bucket, Key}, Payload) ->
    gen_fsm:start_link(?MODULE,
        [ReqID, From, Client, {Bucket, Key}, Payload], []).

write(Client, {Bucket, Key}, Payload) ->
    lager:warning("Client: ~p ~n", [Client]),
    lager:warning("Context: ~p ~n", [{Bucket, Key}]),
    ReqID = mk_reqid(),

    sdgh_write_fsm_sup:start_write_fsm(
        [ReqID, self(), Client, {Bucket, Key}, Payload]),
    {ok, ReqID}.

%% States

%% @doc Prepare the write by calculating the _preference list_.
prepare(timeout, SD0=#state{context={Bucket,Key}}) ->
    lager:warning("In _prepare_ state: ~n ~p ~n~n", [SD0]),
    %% this is controlled by the function set in the tagged tuple
    %% containing `chash_keyfun` with the `default_bucket_props` within
    %% the `app.config` file within a release. Right now it's set to
    %% the bucket-only function in `riak_core_util` module:
    %%
    %% {chash_keyfun, {riak_core_util, chash_bucketonly_keyfun}
    DocIdx = riak_core_util:chash_key({Bucket,
                                       Key}),
    %% the third argument is the "UpNodes", this is an atom here,
    %% `sdgh`, and will change with each app.
    %% How would you know the atom to pass? It will be used in the
    %% module implementing the OTP application behavior, which for
    %% this app is `sdgh_app.erl` and here's the line:
    %%      riak_core_node_watcher:service_up(sdgh, self())
    Preflist = riak_core_apl:get_apl(DocIdx, ?N, sdgh),
    SD = SD0#state{preflist=Preflist},
    {next_state, execute, SD, 0}.

%% @doc Execute the write request and then go into waiting state to
%% verify it has meets consistency requirements.
execute(timeout, SD0=#state{req_id=ReqID,
                            context={Bucket,Key},
                            payload=Payload,
                            preflist=Preflist}) ->
    lager:warning("In _execute_ state: ~n ~p ~n~n", [SD0]),
    lager:warning("sdgh_write_fsm pid is: ~p~n~n", [self()]),
    sdgh_vnode:write_event(Preflist, ReqID, {Bucket,Key}, Payload),
    {next_state, waiting, SD0}.

%% @doc Wait for W write reqs to respond.
waiting({ok, ReqID}, SD0=#state{from=From, num_w=NumW0}) ->
    lager:warning("In _waiting_ state: ~p : ~n ~p ~n~n", [{ok,ReqID},SD0]),
    NumW = NumW0 + 1,
    SD = SD0#state{num_w=NumW},
    if
        NumW =:= ?W ->
            From ! {ReqID, ok},
            {stop, normal, SD};
        %% "true" case - so when we're not equal to our W value
        true ->
            {next_state, waiting, SD}
    end;
waiting(Msg, SD0) ->
    lager:warning("In _waiting_ state: ~p : ~n ~p ~n~n", [Msg,SD0]),


    {next_state, waiting, SD0}.

%% Callbacks

%% @doc Initialize the state data.
init([ReqID, From, Client, {Bucket, Key}, Payload]) ->
    % initial the state data that will be passed along to the FSM
    SD = #state{req_id=ReqID,
                from=From,
                client=Client,
                context={Bucket, Key},
                payload=Payload},
    lager:warning("Initializing... ~n~n state:~n ~p~n~n"),
    {ok, prepare, SD, 0}.

handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    lager:warning("Terminate called... ~p : ~p ~n ~p ~n~n",
            [_Reason, _SN, _SD]),
    ok.

%% Internal Functions

mk_reqid() ->
    erlang:phash2(erlang:now()).