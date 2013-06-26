%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(mnsr_event_proc).

-export([
    init/1,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    process_post/2
    ]).

-export([
    to_text/2
    ]).

-include_lib("webmachine/include/webmachine.hrl").

-define(ROOT_DIR, "/tmp/mnsr/").

-record(ctx, {mod, func}).

%% Initialization for the resource.
%%
%% Create a record, ``ctx``, for a context to be passed through to each
%% method in the webmachine resource lifecycle.
init([]) ->
    Module = sdgh,
    Func = write,
    Result = Module:start_link(),
    lager:warning("do we mean even make it even? Did the module start? ~p~n~n",
        [Result]),
    Ctx = #ctx{mod=Module,func=Func},
    {ok, Ctx}.


%% define the allowed HTTP methods
%%
%% Rd - request data, an opaque record, #wm_reqdata
%% Ctx - resource's context that is provided
allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.


content_types_accepted(RD, Ctx) ->
    {[{"text/plain", accept_plain}], RD, Ctx}.


content_types_provided(RD, Ctx) ->
    {[{"text/plain", to_text}], RD, Ctx}.


to_text(RD, Ctx = #ctx{mod=Module, func=Func}) ->
    %% keys in the path_info list: bucket, keyish
    Bucket = wrq:path_info(bucket, RD),
    Key = wrq:path_info(keyish, RD),
    Data = wrq:req_body(RD),
    Result = Module:Func(?MODULE, {Bucket, Key}, Data),
    lager:warning("We tried to write... ~p~n~n",
        [Result]),
    {<<"{\"success\": true }">>, RD, Ctx}.


process_post(RD, Ctx) ->
    {RespContent, _, _} = to_text(RD, Ctx),
    Resp0 = wrq:set_resp_header("Content-Type", "application/json", RD),
    Resp1 = wrq:set_resp_body(RespContent, Resp0),
    {true, Resp1, Ctx}.
