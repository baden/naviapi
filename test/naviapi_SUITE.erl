-module(naviapi_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [
    {group, noauth},
    {group, auth}
].

groups() ->
    [
        {noauth, [parallel], [test1, test2]},
        {auth, [parallel], [test3]}
    ].

-define(API_PORT, 8982).

init_per_suite(Config) ->
    error_logger:tty(false),

    {ok, Modules} = application:ensure_all_started(naviapi),
    {ok, GunModules} = application:ensure_all_started(gun),

    [{modules, Modules ++ GunModules} | Config].

end_per_suite(Config) ->
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    application:unload(lager), application:unload(navidb), application:unload(naviapi),
    error_logger:tty(true),
    ok.

% init_per_testcase(_Case, Config) ->
%     % TODO: Authorize here?
%     Config.
%
% end_per_testcase(_Case, Config) ->
%     Config.

init_per_group(noauth, Config) ->
    Config;

init_per_group(auth, Config) ->
    % TODO: Authorize here?
    Username = <<"baden">>,
    {200, Headers, Body} = apipost("/1.0/auth", #{
                                                    grant_type => <<"password">>,
                                                    username   => Username,
                                                    password   => <<"111">>
                                                }
                                  ),
    Content_Type = proplists:get_value(<<"content-type">>, Headers),
    ?assertEqual(<<"application/json; charset=utf-8">>, Content_Type),

    #{<<"access_token">> := Access_Token} = Body,
    % ct:pal("Access_Token = ~p", [Access_Token]),

    [{token, Access_Token}, {username, Username} | Config].

end_per_group(noauth, Config) ->
    Config;

end_per_group(auth, Config) ->
    Config.

test1(_Config) ->
    % Out of REST API
    {200, _, _} = apiget("/1.0/info"),
    ok.

test2(_Config) ->
    Username = <<"baden">>,
    {401, _, _} = apiget("/1.0/account"),  % Not authorize jet
    {200, Headers, Body} = apipost("/1.0/auth", #{
                                                    grant_type => <<"password">>,
                                                    username   => Username,
                                                    password   => <<"111">>
                                                }
                                  ),
    Content_Type = proplists:get_value(<<"content-type">>, Headers),
    ?assertEqual(<<"application/json; charset=utf-8">>, Content_Type),

    % TODO: simple base64 regexp
    Cookie = proplists:get_value(<<"set-cookie">>, Headers),
    % <<"access_token=itpek0KqRHex0YoUPf9flIlUCzkmgea8; Version=1; Expires=Wed, 11-Nov-2015 14:58:30 GMT; Max-Age=31536000; Path=/">>
    {ok, RE_Cookie} = re:compile("access_token=([0-9A-Za-z+/]+)"),
    {match, [Token1]} = re:run(Cookie, RE_Cookie, [{capture, [1], list}]),
    CookieToken = list_to_binary(Token1),

    #{<<"access_token">> := Access_Token} = Body,

    ?assertEqual(CookieToken, Access_Token),

    {200, _, AccountBody} = apiget("/1.0/account", [{<<"authorization">>, <<"Bearer ", Access_Token/binary>>}]),
    ?assertMatch(#{<<"username">> := Username}, AccountBody),
    % TODO: Test cookies authorization
    ok.


test3(Config) ->
    Username = ?config(username, Config),
    Token = ?config(token, Config),

    % GET before
    ?assertMatch({200, _, #{<<"username">> := Username}}, apiget("/1.0/account", {token, Token})),

    RandomValue = random_string(),
    % PATCH
    {200, _, RespBody} = apipatch("/1.0/account", {token, Token}, #{<<"foo">> => RandomValue}),
    ct:pal("RespBody = ~p", [RespBody]),

    % GET after
    ?assertMatch(
        {200, _, #{<<"username">> := Username, <<"foo">> := RandomValue}},
        apiget("/1.0/account", {token, Token})
    ),

    ok.

% Private

apiget(Url) ->
    apiget(Url, []).

apiget(Url, Headers) ->
    {ok, ConnPid} = gun:open("localhost", ?API_PORT, [{retry, 0}, {type, tcp}]),
    Ref = gun:get(ConnPid, Url, header(Headers)),
    Response = case gun:await(ConnPid, Ref) of
        {response, nofin, Status, RespHeaders} ->
            {ok, Body} = gun:await_body(ConnPid, Ref),
            case proplists:get_value(<<"content-type">>, RespHeaders, undefined) of
                <<"application/json; charset=utf-8">> ->
                    {Status, RespHeaders, jsxn:decode(Body)};
                _ ->
                    {Status, RespHeaders, Body}
            end;
        {response, fin, Status, RespHeaders} ->
            {Status, RespHeaders, <<"">>}
    end,
    gun:close(ConnPid),
    Response.

apipost(Url, Payload) ->
    apipost(Url, [], Payload).

apipost(Url, Headers, Payload) ->
    {ok, ConnPid} = gun:open("localhost", ?API_PORT, [{retry, 0}, {type, tcp}]),
    PostHeaders = [{<<"content-type">>, <<"application/json; charset=utf-8">>} | header(Headers)],
    Data = jsxn:encode(Payload),
    Ref = gun:post(ConnPid, Url, PostHeaders, Data),
    Response = case gun:await(ConnPid, Ref) of
        {response, nofin, Status, RespHeaders} ->
            {ok, Body} = gun:await_body(ConnPid, Ref),
            case proplists:get_value(<<"content-type">>, RespHeaders, undefined) of
                <<"application/json; charset=utf-8">> ->
                    {Status, RespHeaders, jsxn:decode(Body)};
                _ ->
                    {Status, RespHeaders, Body}
            end;
        {response, fin, Status, RespHeaders} ->
            {Status, RespHeaders, <<"">>}
    end,
    gun:close(ConnPid),
    Response.

apipatch(Url, Headers, Payload) ->
    {ok, ConnPid} = gun:open("localhost", ?API_PORT, [{retry, 0}, {type, tcp}]),
    PostHeaders = [{<<"content-type">>, <<"application/json; charset=utf-8">>} | header(Headers)],
    Data = jsxn:encode(Payload),
    Ref = gun:patch(ConnPid, Url, PostHeaders, Data),
    Response = case gun:await(ConnPid, Ref) of
        {response, nofin, Status, RespHeaders} ->
            {ok, Body} = gun:await_body(ConnPid, Ref),
            case proplists:get_value(<<"content-type">>, RespHeaders, undefined) of
                <<"application/json; charset=utf-8">> ->
                    {Status, RespHeaders, jsxn:decode(Body)};
                _ ->
                    {Status, RespHeaders, Body}
            end;
        {response, fin, Status, RespHeaders} ->
            {Status, RespHeaders, <<"">>}
    end,
    gun:close(ConnPid),
    Response.

header({token, Token}) ->
    [{<<"authorization">>, <<"Bearer ", Token/binary>>}];

header(Any) ->
    Any.

random_string() ->
    base64:encode(crypto:rand_bytes(16)).
