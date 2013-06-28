-module(sdgh_app).

-behaviour(application).

-include("sdgh.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    SdghResult = case sdgh_sup:start_link() of
        {ok, Pid} ->
            ?PRINT(Pid),
            ok = riak_core:register([{vnode_module, sdgh_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(
                                    sdgh_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(
                                    sdgh_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(sdgh, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end,
    MnsrResult = case mnsr_sup:start_link() of
        {ok, MnsrPid} ->
            {ok, MnsrPid};
        {error, MnsrReason} ->
            {error, MnsrReason}
    end,
    webmachine_router:add_route({["boo"], mnsr_resource, []}),
    webmachine_router:add_route({["event", bucket, keyish], mnsr_event_proc, []}),
    %% This is going to be problematic, the two start sequences and only
    %% reporting the Sourdough start result, this should likely be merged &
    %% reported when there is an error in either case...
    %%
    %% So, if SdghResult = {error, _} or MnsrResult = {error, _} then return
    %% return {error, [(SdghReason|MnsrReason)]}
    ?PRINT(SdghResult),
    ?PRINT(MnsrResult),
    SdghResult.

stop(_State) ->
    ok.