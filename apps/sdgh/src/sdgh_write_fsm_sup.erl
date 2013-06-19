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
    lager:warning("Supervising, like a boss from pid: ~p ~n~n", [self()]),
    lager:warning("Starting write_fsm with: ~n~p~n~n", [Args]),
    Child = supervisor:start_child(?MODULE, Args),
    lager:warning("Response from start_child: ~p~n~n", [Child]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WriteFsm = {undefined,
                {sdgh_write_fsm, start_link, []},
                temporary, 5000, worker, [sdgh_write_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [WriteFsm]}}.