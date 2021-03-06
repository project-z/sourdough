%% @author Andrew Lenards <alenards@kissmetrics.com>
%% @copyright Project-Z 2013.
%% @doc Event processing & persistence resource.

-module(mnsr_event_proc).
-include("sdgh.hrl").

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
-define(WORKER_MOD, sdgh).
-define(WORKER_OP, write).

-record(ctx, {mod, func}).

%% Initialization for the resource.
%%
%% Create a record, ``ctx``, for a context to be passed through to each
%% method in the webmachine resource lifecycle.
init([]) ->
    % sdgh:write/3
    Ctx = #ctx{mod=?WORKER_MOD, func=?WORKER_OP},
    {ok, Ctx};
init([{Mod, Op}]) ->
    Ctx = #ctx{mod=Mod, func=Op},
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


to_text(RD, Ctx = #ctx{mod=Module, func=Op}) ->
    %% keys in the path_info list: bucket, keyish
    Bucket = wrq:path_info(bucket, RD),
    Key = wrq:path_info(keyish, RD),
    Data = wrq:req_body(RD),
    Success = case Module:Op(?MODULE, {Bucket, Key}, Data) of
            ok -> true;
            _  -> false
    end,
    lager:warning("We tried to write... ~p~n~n", [Success]),
    Msg = build_msg(Success, [{Bucket,Key}, Data]),
    {Msg, RD, Ctx}.


process_post(RD, Ctx) ->
    lager:warning("Request: ~p~n~n", [RD]),
    lager:warning("Context: ~p~n~n", [Ctx]),
    {{Success, RespContent}, _, _} = to_text(RD, Ctx),
    Resp0 = wrq:set_resp_header("Content-Type", "application/json", RD),
    Resp1 = wrq:set_resp_body(RespContent, Resp0),
    case Success of
        true ->
            {true, Resp1, Ctx};
        false ->
            {{halt,500}, Resp1, Ctx }
    end.


build_msg(Success, EventData) ->
    case Success of
        true -> {Success, <<"{\"success\": true }">>};
        false -> {Success, list_to_binary(
                    io_lib:format(
                        "{\"success\": false, \"details\": \"Failed with data: ~p \" }",
                            [EventData]))}
    end.

%%===================================================================
%% Test code
%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%% this is used in #wm_reqdata record...
-include_lib("webmachine/include/wm_reqstate.hrl").


%% use file:cwd() ++ "sdgh" to get where the write will occur...
verify_init_test() ->
    {ok, C0} = init([]),
    %% we expect the default module:func to be sdgh:write, for ease of modification
    %% we'll defined them as macros, ?WORKER_MOD:?WORKER_OP
    ?WORKER_MOD = C0#ctx.mod,
     ?WORKER_OP = C0#ctx.func.


allowed_methods_test() ->
    ReqData = #wm_reqdata{},
    Ctx = #ctx{},
    {['POST'], ReqData, Ctx} = allowed_methods(ReqData, Ctx).


build_msg_test() ->
    {true, Msg1} = build_msg(true, []),
    {false, Msg2} = build_msg(false, [goat, goat, goat]),
    Msg1 =/= Msg2.


process_post_test() ->
    {ok, C1} = init([{mock_vnode, mock_write}]),
    mock_vnode = C1#ctx.mod,
    mock_write = C1#ctx.func,

    %%  We have the HTTP Method, Scheme (http or https), version, the URL path,
    %%  and any headers that we might need for our request record:
    %%
    %%   create(Method, Scheme, {1,1}, RawPath, mochiweb_headers:from_list(Headers))
    %%
    R0 = wrq:create('POST', http, {1,1}, "/event/tauntauncoffee/hoth1",
                  mochiweb_headers:from_list([])),
    R1 = wrq:set_peer("127.0.0.1", R0),
    D0 = dict:new(),
    D1 = dict:store(bucket, "tauntauncoffee", D0),
    D2 = dict:store(keyish, "hoth1", D1),
    Payload = <<"There's something outside... IT'S METAL!!!!">>,
    RS = #wm_reqstate{bodyfetch=standard},
    R2 = R1#wm_reqdata{path_info=D2, wm_state=RS},
    RD = wrq:set_req_body(Payload, R2),
    ok = mock_vnode:mock_write(?MODULE,{"tauntauncoffee","hoth1"},undefined),
    %% for giggles, lets's verify our mock function so `erlc` will shut up
    ok = mock_vnode:mock_write("ha", {"allthe","tuples"}, Payload),

    %% let's verify that we actually built the RD (Request Data) correctly
    "hoth1" = wrq:path_info(keyish, RD),
    "tauntauncoffee" = wrq:path_info(bucket, RD),

    {Msg, RD, _C2} = to_text(RD, C1),
    ?debugFmt("msg: ~p~n~n", [Msg]),
    {true, _More} = Msg.


-endif.