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

% -define(API_PORT, 8982).
-define(HOST, "localhost").
% -define(PORT, ?config(port, Config)).

init_per_suite(Config) ->
    error_logger:tty(false),

    {ok, Modules} = application:ensure_all_started(naviapi),
    {ok, GunModules} = application:ensure_all_started(gun),
    {ok, Port} = application:get_env(naviapi, port),
    [{modules, Modules ++ GunModules}, {host, ?HOST}, {port, Port} | Config].

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
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    % TODO: Authorize here?
    Username = <<"baden">>,
    {200, Headers, Body} = helper:post(Host, Port, "/1.0/auth", #{
                                                    grant_type => <<"password">>,
                                                    username   => Username,
                                                    password   => <<"111">>
                                                }
                                  ),
    Content_Type = proplists:get_value(<<"content-type">>, Headers),
    ?assertEqual(<<"application/json; charset=utf-8">>, Content_Type),

    #{<<"access_token">> := Access_Token} = Body,

    [{token, Access_Token}, {username, Username} | Config].

end_per_group(noauth, Config) ->
    Config;

end_per_group(auth, Config) ->
    Config.

test1(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    % Out of REST API
    {200, _, _} = helper:get(Host, Port, "/1.0/info"),
    ok.

test2(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    Username = <<"baden">>,
    {401, _, _} = helper:get(Host, Port, "/1.0/account"),  % Not authorize jet
    {200, Headers, Body} = helper:post(Host, Port, "/1.0/auth", #{
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

    {200, _, AccountBody} = helper:get(Host, Port, "/1.0/account", [{<<"authorization">>, <<"Bearer ", Access_Token/binary>>}]),
    ?assertMatch(#{<<"username">> := Username}, AccountBody),
    % TODO: Test cookies authorization
    ok.


test3(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    Username = ?config(username, Config),
    Token = ?config(token, Config),

    % GET before
    ?assertMatch(
        {200, _, #{<<"username">> := Username}},
        helper:get(Host, Port, "/1.0/account", {token, Token})
    ),

    RandomValue = helper:random_string(),
    % PATCH
    {200, _, RespBody} = helper:patch(Host, Port, "/1.0/account", {token, Token}, #{<<"foo">> => RandomValue}),
    ct:pal("RespBody = ~p", [RespBody]),

    % GET after
    ?assertMatch(
        {200, _, #{<<"username">> := Username, <<"foo">> := RandomValue}},
        helper:get(Host, Port, "/1.0/account", {token, Token})
    ),

    ok.

% Private
