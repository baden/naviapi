%% -*- coding: utf-8 -*-
-module(naviapi_rest).
-author('Denis Batrak <baden.i.ua@gmail.com>').

% -behaviour(cowboy_sub_protocol).
-export([
        upgrade/6
    ]).

-export([
        rest_init/2,
        rest_terminate/3,
        is_authorized/2,
        forbidden/2,
        allowed_methods/2,
        content_types_provided/2,
        charsets_provided/2,
        content_types_accepted/2,
        delete_resource/2,
        delete_completed/2
    ]).

% getters
-export([
        get_resource/2
]).

% setters
-export([
        put_json/2
]).

-export([
        cors/1,
        is_admin/1
        ]).

% TODO: Перенести в erlnavicc.hrl для возможности доступа из API?
-type proplist() :: list({term(), term()}).

-record(state, {
                username    :: binary(),
                method      :: binary(),
                headers     :: map(),
                body        :: proplist(),
                params      :: map(),
                querystring :: map(),
                options     :: map(),
                cookies     :: map(),
                handler     :: module(),
                handler_state :: map(),
                completed = false :: boolean()
         }).

upgrade(Req, Env, Handler, HandlerState, infinity, run) ->
    Req1 = cors(Req),

    Method = cowboy_req:method(Req1),
    Headers = maps:from_list(cowboy_req:headers(Req1)),
    Params = maps:from_list(cowboy_req:bindings(Req1)),
    Query  = maps:from_list(cowboy_req:parse_qs(Req1)),
    Cookies = maps:from_list(cowboy_req:parse_cookies(Req1)),

    Opts = #{},

    % <<"Bearer ", Token/binary>> = maps:get(<<"authorization">>, Headers, <<"Bearer  ">>),
    % io:format("~p@~p[~p]:~p~n", [Method, Handler, Token, maps:merge(Params, Query)]),

    cowboy_rest:upgrade(Req1, Env, ?MODULE, #state{
        username = undefined,
        method = Method,
        % params = lists:ukeymerge(1,
        %     lists:ukeysort(1, Params),
        %     lists:ukeysort(1, Query)),
        headers = Headers,
        params = maps:merge(Params, Query),
        querystring = Query,
        options = Opts,
        cookies = Cookies,
        handler = Handler,
        handler_state = HandlerState
    }, infinity, run).

rest_init(Req, State) ->
    {ok, Req, State}.

rest_terminate(_Reason, _Req, _State) ->
    ok.

is_authorized(Req, State = #state{method = <<"OPTIONS">>}) ->
    {true, Req, State};

is_authorized(Req, State = #state{handler_state = #{auth := false}}) ->
    {true, Req, State#state{username = undefined}};

is_authorized(Req, State) ->
    case get_access_token(State) of
        {ok, Token} ->
            case oauth2:verify_access_token(Token, []) of
                {ok, Identity} ->
                    {<<"user">>, Username} = proplists:get_value(<<"resource_owner">>, Identity),
                    {true, Req, State#state{username = Username}};
                {error, access_denied} ->
                    {{false, <<"Bearer">>}, Req, State}
            end;
        {error, _} ->
            {{false, <<"Bearer">>}, Req, State}
    end.

forbidden(Req, State = #state{method = Method, username = Username, handler = Handler}) ->
    {not call_allowed(handler_for(Method), Username, Handler), Req, State}.

handler_for(<<"GET">>) -> get;
handler_for(<<"POST">>) -> post;
handler_for(<<"PUT">>) -> put;
handler_for(<<"PATCH">>) -> patch;
handler_for(<<"DELETE">>) -> delete;
handler_for(<<"OPTIONS">>) -> options;
handler_for(<<"HEAD">>) -> get;
handler_for(Other) -> Other.

call_allowed(Method, Username, Handler) ->
  case erlang:function_exported(Handler, allowed, 2) of
    true ->
      Handler:allowed(Method, Username);
    false ->
      true
  end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>,
        <<"PATCH">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_resource},
        {{<<"application">>, <<"octet-stream">>, []}, get_resource}
    ], Req, State}.

charsets_provided(Req, State) ->
    case cowboy_req:header(<<"accept">>, Req) of
        <<"application/octet-stream">> ->
            no_call;
        _ ->
            {[<<"utf-8">>], Req, State}
    end.

content_types_accepted(Req, State) ->
    % ?INFO("rest_resourse:content_types_accepted (~p)", [State]),
    {[{{<<"application">>, <<"json">>, '*'}, put_json}], Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_access_token(#state{headers = #{<<"authorization">> := <<"Bearer ", Token/binary>>}}) ->
    {ok, Token};

get_access_token(#state{params = #{<<"access_token">> := Token}}) ->
    {ok, Token};

get_access_token(#state{cookies = #{<<"access_token">> := Token}}) ->
    {ok, Token};

get_access_token(_State) ->
    {error, missing}.


put_json(Req, State) ->
    % @todo make it streaming
    {ok, Body, Req2} = cowboy_req:body(Req),
    case jsxn:decode(Body, [{error_handler, fun(_, _, _) -> {error, badarg} end}])
    of
        {error, _} ->
            {false, Req2, State};
        {incomplete, _} ->
            {false, Req2, State};
        Data ->
            put_resource(Req2, State#state{body = Data})
    end.


get_resource(Req, State = #state{params = Params, handler = Handler, options = Opts, username = Username}) ->
    try Handler:get(Params, Opts#{username => Username}) of
        {ok, Result} ->
            {serialize(Result, Req), Req, State};
        {N, Result} when is_integer(N) ->
            {halt, respond(N, Result, Req), State};
        {error, enoent} ->
            {halt, respond(404, undefined, Req), State};
        {error, Reason} ->
            {halt, respond(400, Reason, Req), State};
        error ->
            {halt, respond(400, undefined, Req), State};
        {goto, Location = << $/, _/binary >> } ->
            {halt, cowboy_req:set_resp_header(<<"location">>, Location, Req), State};
        {goto, Location} ->
            BasePath = cowboy_req:path(Req),
            {halt, cowboy_req:set_resp_header(<<"location">>,
                [BasePath, $/, Location], Req), State}
    catch Class:Reason ->
        lager:error(
            "** API handler ~p terminating in get/3~n"
            "   for the reason ~p:~p~n** State was ~p~n"
            "** Stacktrace: ~p~n~n",
            [Handler, Class, Reason, State, erlang:get_stacktrace()]),
            io:format("Backtrace = ~p~n", [erlang:get_stacktrace()]),
        {halt, respond(500, Reason, Req), State}
    end.

put_resource(Req, State = #state{method = <<"POST">>, body = Data,
    params = Params, handler = Handler, options = Opts, username = Username}) ->
    % ?INFO("rest_resourse:put_resource (~p)", [State]),
    try Handler:post(Data, Params, Opts#{username => Username}) of
        {ok, Body} ->
            {true, set_resp_body(Body, Req), State};
        {ok, Body, Callback} ->
            Req1 = Callback(Req),
            {true, set_resp_body(Body, Req1), State};
        ok ->
            {true, Req, State};
        {N, Result} when is_integer(N) ->
            {halt, respond(N, Result, Req), State};
        {error, eexist} ->
            {halt, respond(409, <<"eexist">>, Req), State};
        {error, Reason} ->
            {halt, respond(400, Reason, Req), State};
        error ->
            {halt, respond(400, undefined, Req), State};
        {goto, Location = << $/, _/binary >> } ->
            {{true, Location}, Req, State};
        {goto, Location} ->
            BasePath = cowboy_req:path(Req),
            {{true, [BasePath, $/, Location]}, Req, State}
    catch Class:Reason ->
        lager:error(
            "**** API handler ~p terminating in post/3~n"
            "     for the reason ~p:~p~n** State was ~p~n"
            "     Stacktrace: ~p~n~n",
            [Handler, Class, Reason, State, erlang:get_stacktrace()]),
            io:format("Backtrace = ~p~n", [erlang:get_stacktrace()]),
        {halt, respond(500, Reason, Req), State}
  end;

put_resource(Req, State = #state{method = <<"PUT">>, body = Data,
    params = Params, handler = Handler, options = Opts, username = Username}) ->
  try Handler:put(Data, Params, Opts#{username => Username}) of
    ok ->
      {true, Req, State};
    {ok, Body} ->
      {true, set_resp_body(Body, Req), State};
    {N, Result} when is_integer(N) ->
        {halt, respond(N, Result, Req), State};
    {error, eexist} ->
      {halt, respond(409, <<"eexist">>, Req), State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State};
    error ->
      {halt, respond(400, undefined, Req), State}
  catch Class:Reason ->
    error_logger:error_msg(
      "** API handler ~p terminating in put/3~n"
      "   for the reason ~p:~p~n** State was ~p~n"
      "** Stacktrace: ~p~n~n",
      [Handler, Class, Reason, State, erlang:get_stacktrace()]),
      io:format("Backtrace = ~p~n", [erlang:get_stacktrace()]),
    {halt, respond(500, Reason, Req), State}
  end;

put_resource(Req, State = #state{method = <<"PATCH">>, body = Data,
    params = Params, handler = Handler, options = Opts, username = Username}) ->
  try Handler:patch(Data, Params, Opts#{username => Username}) of
    ok ->
      {true, Req, State};
    {ok, Body} ->
      {true, set_resp_body(Body, Req), State};
    {error, enoent} ->
      {halt, respond(404, <<"enoent">>, Req), State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State};
    error ->
      {halt, respond(400, undefined, Req), State}
  catch Class:Reason ->
    error_logger:error_msg(
      "** API handler ~p terminating in patch/3~n"
      "   for the reason ~p:~p~n** State was ~p~n"
      "** Stacktrace: ~p~n~n",
      [Handler, Class, Reason, State, erlang:get_stacktrace()]),
      io:format("Backtrace = ~p~n", [erlang:get_stacktrace()]),
    {halt, respond(500, Reason, Req), State}
  end.


%%
%% Delegates actual processing to application's delete/2 handler.
%%
%% It should start deleting the resource and return.
%% - {true, Req, State} --> 204 No Content, unless delete_completed/2 defined
%% - {X =/= true, Req, State} --> 500 Internal Server Error
%% - {halt, Req, State} --> no further processing
%%
delete_resource(Req, State = #state{
    params = Params, handler = Handler, options = Opts, username = Username}) ->
  try Handler:delete(Params, Opts#{username => Username}) of
    ok ->
      {true, Req, State#state{completed = true}};
    accepted ->
      {true, Req, State#state{completed = false}};
    error ->
      {halt, respond(400, undefined, Req), State};
    {error, enoent} ->
      {halt, respond(404, <<"enoent">>, Req), State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State}
  catch Class:Reason ->
    error_logger:error_msg(
      "** API handler ~p terminating in delete/3~n"
      "   for the reason ~p:~p~n** State was ~p~n"
      "** Stacktrace: ~p~n~n",
      [Handler, Class, Reason, State, erlang:get_stacktrace()]),
    {halt, respond(500, Reason, Req), State}
  end.

%%
%% Indicates whether the resource has been deleted yet.
%% - {true, Req, State} --> go ahead with 200/204
%% - {false, Req, State} --> 202 Accepted
%% - {halt, Req, State} --> no further processing
%%
delete_completed(Req, State = #state{completed = Completed}) ->
  {Completed, Req, State}.


%%
%% Error reporting.
%%
reason(undefined) ->
  reason(<<"unknown">>);
reason(Reason) when is_list(Reason) ->
  Reason;
reason(Reason) when is_map(Reason) ->
  Reason;
reason(Reason) when is_binary(Reason); is_number(Reason) ->
  reason([{error, Reason}]);
reason(Reason) when is_atom(Reason) ->
  reason(atom_to_binary(Reason, latin1)).


respond(Status, Reason, Req) ->
    % io:format("--------------------~nStatus = ~p~nReason = ~p~n-----------------~n", [Status, Reason]),
    % Reply = cowboy_req:reply(Status, set_resp_body(reason(Reason), Req)),
    % io:format("---------------~nReply = ~p~n------------------~n", [Reply]),
    % % {ok, Req2} = cowboy_req:reply(Status, set_resp_body(reason(Reason), Req)),
    % {ok, Req2} = Reply,
    % Req2.
    cowboy_req:reply(Status, set_resp_body(reason(Reason), Req)).

set_resp_body(Body, Req) ->
    % ?INFO("rest_resourse:set_resp_body (~p)", [Body]),
    cowboy_req:set_resp_body(serialize(Body, Req), Req).


serialize(Body, Req) ->
    % NB: we choose encoder from media_type meta, honoring Accept: header.
    % One may choose to always encode to one fixed format as well.
    CType = cowboy_req:meta(media_type, Req),
    encode(CType, Body, Req).

%% NB: first argument should match those of content_types_*/2
encode({<<"application">>, <<"octet-stream">>, _Params}, Body, _Req) ->
  Body;

encode({<<"application">>, <<"x-www-form-urlencoded">>, _Params}, Body, _Req) ->
  build_qs(Body);

encode({<<"application">>, <<"json">>, _Params}, Body, _Req) ->
    Res = jsxn:encode(Body, [space, {indent, 4}]),
    Res;

encode({<<"application">>, <<"rpc+json">>, _Params}, Body, _Req) ->
    jsxn:encode(Body, [space, {indent, 4}]).


%% NB: Cowboy issue #479
build_qs(Bin) when is_binary(Bin) ->
  cowboy_http:urlencode(Bin);
build_qs(Atom) when is_atom(Atom) ->
  build_qs(atom_to_binary(Atom, latin1));
build_qs(Int) when is_integer(Int) ->
  % NB: nothing unsafe in integers
  list_to_binary(integer_to_list(Int));
build_qs({K, undefined}) ->
  << (build_qs(K))/binary, $= >>;
build_qs({K, V}) ->
  << (build_qs(K))/binary, $=, (build_qs(V))/binary >>;
build_qs([]) ->
  <<>>;
build_qs(List) when is_list(List) ->
  << "&", R/binary >> = << << "&", (build_qs(X))/binary >> || X <- List >>,
  R.

%%
%% Setup CORS
%%
cors(Req) ->
    % {ok, Origin} = application:get_env(erlnavicc, origin),
    {ok, Origin} = application:get_env(naviapi, origin),
    NewOrigin = case cowboy_req:header(<<"origin">>, Req) of
        undefined -> Origin;
        OriginFromReq -> OriginFromReq
    end,
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, [NewOrigin],
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Credentials">>, <<"true">>,
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"OPTIONS, GET, POST, PUT, PATCH, DELETE">>,
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"content-type, if-modified-since, authorization, x-requested-with">>, Req)))).

is_admin(Username) ->
    #{groups := Groups} = navidb:get(accounts, {username, Username}),
    lists:member(<<"admin">>, Groups).
