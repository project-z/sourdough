-module(sdgh_sup).

-behaviour(supervisor).

-include("sdgh.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = {sdgh_vnode_master,
                  {riak_core_vnode_master, start_link, [sdgh_vnode]},
                  permanent, 6500, worker, [riak_core_vnode_master]},

    WriteFSMs = {sdgh_write_fsm_sup,
                    {sdgh_write_fsm_sup, start_link, []},
                    permanent, infinity, supervisor, [sdgh_write_fsm_sup]},
    {ok,
        {{one_for_one, 5, 10},
          [VMaster, WriteFSMs]}}.
