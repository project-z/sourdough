%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(mnsr_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    lager:warning("the world hates me... wtf?"),
    {ok, undefined}.

to_html(ReqData, State) ->
    Time = now(),
    lager:warning("Do you have the time? ~p~n~n", [Time]),
    Content = "<html><body>Hello, something new...  </body></html>",
        % ++ lists:flatten(io_lib:format("~p", [Time]))
        % ++ "</body></html>",
    {Content, ReqData, State}.
