-module(helper).

-export([get/3, get/4, post/4, post/5, patch/5, random_string/0]).

get(Host, Port, Url) ->
    get(Host, Port, Url, []).

get(Host, Port, Url, Headers) ->
    {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
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

post(Host, Port, Url, Payload) ->
    post(Host, Port, Url, [], Payload).

post(Host, Port, Url, Headers, Payload) ->
    {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
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

patch(Host, Port, Url, Headers, Payload) ->
    {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
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
