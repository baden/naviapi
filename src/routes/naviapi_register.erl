%% -*- coding: utf-8 -*-
-module(naviapi_register).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            options/2,
            rest_init/2,
            allowed_methods/2
        ]).

-export([
            content_types_provided/2,
            charsets_provided/2,
            content_types_accepted/2
        ]).

-export([
            process_get/2,
            process_post/2,
            process_post_json/2
        ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"OPTIONS, POST">>, naviapi_rest:cors(Req)),
    {ok, Req1, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

content_types_provided(Req, State) ->
    Req1 = naviapi_rest:cors(Req),
    {[
        {{<<"application">>, <<"json">>, []}, process_get},
        {{<<"text">>, <<"html">>, []}, process_get}
    ], Req1, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, process_post_json},
      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, process_post}],
     Req, State}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>,<<"POST">>], Req, State}.

process_get(Req, State) ->
    {ok, _Body, Req2} = cowboy_req:body(Req),
    Reply = cowboy_req:reply(400, [], <<"Bad Request.">>, Req2),
    {stop, Reply, State}.

process_post_json(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Params = jsx:decode(Body),
    % % {ok, Reply} = process(Req2, Params, State),
    % Res1 = process(Req2, Params, State),
    % io:format("------------~nRes1 = ~p~n------------~n", [Res1]),
    % % {ok, Reply} = Res1,
    % Reply = Res1,
    {stop, process(Req2, Params, State), State}.

process_post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Params = decode_form(Body),
    {ok, Reply} = process(Req2, Params, State),
    {stop, Reply, State}.

% TODO: maps
process(Req, Params, _State) ->
    Username        = proplists:get_value(<<"username">>, Params),
    Password        = proplists:get_value(<<"password">>, Params),
    Title = case proplists:get_value(<<"title">>, Params) of
        undefined -> Username;
        Other -> Other
    end,
    Email           = proplists:get_value(<<"email">>, Params, <<"">>),
    Groupname       = proplists:get_value(<<"groupname">>, Params),
    Grouppassword   = proplists:get_value(<<"grouppassword">>, Params),
    Newgroup        = proplists:get_value(<<"newgroup">>, Params, false),

    % Для начала проверим, возможно пользователь уже существует
    {Code, Result} = case navidb:get(accounts, {username, Username}) of
        #{error := no_entry} ->    % Все в порядке, такого пользователя нет
            % Теперь если определено поле Groupname, то создадим группу
            case Groupname of
                undefined ->    % Не создавать группу
                    Salt = random:uniform(trunc(math:pow(2,64))),
                    Document = #{
                        id       => base64:encode(<<Username/binary, $:, Salt:64>>),
                        username => Username,
                        password => Password,
                        title    => Title,
                        email    => Email,
                        date     => unixtime(),
                        skeys    => [],
                        groups   => []
                    },
                    navidb:insert(accounts, Document),
                    {200, [
                        {username, Username},
                        {title, Title}
                    ]};
                _ ->
                    % Теперь создадим группу для пользователя (или получим существующую)
                    case getgroup(Groupname, Grouppassword, Newgroup) of
                        {error, duplicate} ->
                            {409, [
                                {message, <<"Validation Failed">>},
                                {errors, [[
                                    {code, <<"exist">>},
                                    {field, <<"groupname">>},
                                    {resource, <<"Group">>}
                                ]]}
                            ]};
                        {error, nogroup} ->
                            {404, [
                                {message, <<"Validation Failed">>},
                                {errors, [[
                                    {code, <<"notfound">>},
                                    {field, <<"groupname">>},
                                    {resource, <<"Group">>}
                                ]]}
                            ]};
                        {error, wrongpassword} ->
                            {409, [
                                {message, <<"Validation Failed">>},
                                {errors, [[
                                    {code, <<"wrongpassword">>},
                                    {field, <<"grouppassword">>},
                                    {resource, <<"Group">>}
                                ]]}
                            ]};
                        _Group ->
                            Salt = random:uniform(trunc(math:pow(2,64))),
                            Document = #{
                                id       => base64:encode(<<Username/binary, $:, Salt:64>>),
                                username => Username,
                                password => Password,
                                title    => Title,
                                email    => Email,
                                date     => unixtime(),
                                skeys    => [],
                                groups   => [Groupname]
                            },
                            navidb:insert(accounts, Document),

                            % А также добавим пользователя в группу
                            navidb:update(groups, {groupname, Groupname}, #{'$push' => #{'members' => Username}}),
                            % $addToSet ?

                            {200, [
                                {username, Username},
                                {title, Title},
                                {groupname, Groupname},
                                {newgroup, Newgroup}
                            ]}
                    end
            end;

        _ ->                 % Пользователь уже существует
            {409, [
                {message, <<"Validation Failed">>},
                {errors, [[
                    {code, <<"exist">>},
                    {field, <<"user">>},
                    {resource, <<"GroupMember">>}
                ]]}
            ]}
    end,

    cowboy_req:reply(Code, [], jsxn:encode(Result, [space, {indent, 4}]), Req).

% получить группу
getgroup(Groupname, Grouppassword, false) ->
    % Для начала проверим есть ли такая группа в принципе
    case navidb:get(groups, {groupname, Groupname}) of
        #{error := no_entry} ->    % Такой группы нет вовсе
            {error, nogroup};
        #{grouppassword := Grouppassword} = Group ->                % Группа есть, проверим проверочное слово
            Group;
        _ ->
            {error, wrongpassword}
    end;

% создать группу
getgroup(Groupname, Grouppassword, true) ->
    % Для начала проверим есть ли такая группа
    case navidb:get(groups, {groupname, Groupname}) of
        #{error := no_entry} -> % Все хорошо, такой группы нет, можно создать запись
            Document = #{
                groupname     => Groupname,
                grouppassword => Grouppassword,
                date          => unixtime()
            },
            navidb:insert(groups, Document);
        _ ->  % Такая группа уже есть
            {error, duplicate}
    end.

%%%===================================================================
%%% Grant type handlers
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode_form(Form) ->
    RawForm = cowboy_http:urldecode(Form),
    Pairs = binary:split(RawForm, <<"&">>, [global]),
    lists:map(fun(Pair) ->
                      [K, V] = binary:split(Pair, <<"=">>),
                      {K, V}
              end,
              Pairs).

% unixtime() -> timer:now_diff(now(), {0,0,0}) div 1000000.
unixtime() ->
        {A, B, _} = os:timestamp(),
        (A * 1000000) + B.
