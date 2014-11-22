-module(helper).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start/1, stop/1, auth/1, clean/1, tracker/2, get/2, get/3, post/3, patch/3, put/3, delete/2, random_string/0, escape_uri/1]).

-define(API_VERSION, "1.0").

start(Config) ->
    error_logger:tty(false),

    {ok, Modules} = application:ensure_all_started(naviapi),
    {ok, GunModules} = application:ensure_all_started(gun),
    Host = application:get_env(naviapi, host, "localhost"),
    {ok, Port} = application:get_env(naviapi, port),
    [{modules, Modules ++ GunModules}, {host, Host}, {port, Port} | Config].

stop(Config) ->
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    application:unload(lager), application:unload(navidb), application:unload(naviapi),
    error_logger:tty(true),
    ok.

auth(Config) ->
    Title = random_string(),
    Username = random_string(),
    Password = random_string(),
    % {400, _, _} = helper:get(Host, Port, "/1.0/account"),
    {401, _, _} = helper:get(Config, "/account"),  % Not authorize jet

    {200, _, _} = post(Config, "/register", #{
                                                    grant_type => <<"password">>,
                                                    title      => Title,
                                                    username   => Username,
                                                    password   => Password
    }),
    timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать
    {200, Headers, Body} = post(Config, "/auth", #{
                                                    grant_type => <<"password">>,
                                                    username   => Username,
                                                    password   => Password
    }),
    Content_Type = proplists:get_value(<<"content-type">>, Headers),
    ?assertEqual(<<"application/json; charset=utf-8">>, Content_Type),
    % #{<<"access_token">> := Access_Token} = Body,

    % TODO: simple base64 regexp
    Cookie = proplists:get_value(<<"set-cookie">>, Headers),
    % <<"access_token=itpek0KqRHex0YoUPf9flIlUCzkmgea8; Version=1; Expires=Wed, 11-Nov-2015 14:58:30 GMT; Max-Age=31536000; Path=/">>
    {ok, RE_Cookie} = re:compile("access_token=([0-9A-Za-z+/]+)"),
    {match, [Token1]} = re:run(Cookie, RE_Cookie, [{capture, [1], list}]),
    CookieToken = list_to_binary(Token1),

    #{<<"access_token">> := Access_Token} = Body,

    ?assertEqual(CookieToken, Access_Token),

    [{token, Access_Token}, {username, Username}, {password, Password}, {title, Title} | Config].

clean(Config) ->
    Username = ?config(username, Config),
    navidb:remove(accounts, #{username => Username}),
    Config.

tracker(create, Config) ->
    Imei = helper:random_string(),
    Skey = base64:encode(Imei),
    System = navidb:get(system, Skey, cached), % Это создаст систему
    timer:sleep(50), % Это нужно чтобы пулл базы данных успел записать
    [{imei, Imei}, {skey, Skey}, {system, System} | Config];

tracker(clean, Config) ->
    Skey = ?config(skey, Config),
    navidb:remove(systems, {id, Skey}),
    Config.

get(Config, Url, Params) ->
    [$&|Query] = lists:flatten(maps:fold(
        fun(Key, Value, Acc) ->
            Element = "&" ++ atom_to_list(Key) ++ "=" ++ escape_uri(Value),
            [Element | Acc]
        end,
        [],
        Params
    )),
    get(Config, Url ++ "?" ++ Query).

get(Config, Url) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
    Ref = gun:get(ConnPid, "/" ++ ?API_VERSION ++ Url, header(Config)),
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

post(Config, Url, Payload) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
    PostHeaders = [{<<"content-type">>, <<"application/json; charset=utf-8">>} | header(Config)],
    Data = jsxn:encode(Payload),
    Ref = gun:post(ConnPid, "/" ++ ?API_VERSION ++ Url, PostHeaders, Data),
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

patch(Config, Url, Payload) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
    PostHeaders = [{<<"content-type">>, <<"application/json; charset=utf-8">>} | header(Config)],
    Data = jsxn:encode(Payload),
    Ref = gun:patch(ConnPid, "/" ++ ?API_VERSION ++ Url, PostHeaders, Data),
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

put(Config, Url, Payload) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
    PostHeaders = [{<<"content-type">>, <<"application/json; charset=utf-8">>} | header(Config)],
    Data = jsxn:encode(Payload),
    Ref = gun:put(ConnPid, "/" ++ ?API_VERSION ++ Url, PostHeaders, Data),
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

delete(Config, Url) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
    Ref = gun:delete(ConnPid, "/" ++ ?API_VERSION ++ Url, header(Config)),
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

header(Config) ->
    case proplists:get_value(token, Config) of
        undefined -> [];
        Token -> [{<<"authorization">>, <<"Bearer ", Token/binary>>}]
    end.

random_string() ->
    base64:encode(crypto:rand_bytes(32)).

escape_uri(Data) when is_integer(Data) ->
    escape_uri(integer_to_list(Data));

escape_uri(Data) when is_binary(Data) ->
    escape_uri(binary_to_list(Data));

escape_uri(Data) ->
    edoc_lib:escape_uri(Data).
