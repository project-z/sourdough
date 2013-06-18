%% @doc Supervise the sdgh_write_fsm, a finite state machine for
%% persisting events within Sourdough.
%%
%% Based on Ryan Zezeski's RTS write FSM supervisor
-module(sdgh_write_fsm_sup).
-behaviour(supervisor).
-include("sdgh.hrl").

-export([
    start_write_fsm/1,
    start_link/0]).

-export([
    init/1
    ]).

start_write_fsm(Args) ->
    ?PRINT(Args),
    Child = supervisor:start_child(?MODULE, Args),
    ?PRINT(Child).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WriteFsm = {undefined,
                {sdgh_write_fsm, start_link, []},
                temporary, 5000, worker, [sdgh_write_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [WriteFsm]}}.