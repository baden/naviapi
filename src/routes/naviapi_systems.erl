%% -*- coding: utf-8 -*-
-module(naviapi_systems).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            get/2,
            patch/3
        ]).

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => true}}.  % Для доступа к ресурсу требуется авторизация

get(Query, Options) ->
    get(Query, Options, maps:get(skey, Query, all)).

get(_Query, _Options = #{username := Username}, all) ->
    #{<<"skeys">> := Skeys} = navidb:get(accounts, {username, Username}),
    Result = navidb:get(systems, Skeys),
    {ok, Result};

% Только пользователь группы admin может делать это.
% Возвращает не обычную запись системы, а мета-запись
get(_Query, _Options = #{username := Username}, <<"all">>) ->
    Systems = case naviapi_rest:is_admin(Username) of
        false ->
            [];
        true ->
            navidb:get_all_systems()
    end,

    Result = #{
        id      => <<"all">>,
        systems => Systems
    },
    {ok, Result};

get(_Query, _Options, Skey) ->
    % По идее это тоже нужно перенести в navidb
    case navidb:get(systems, {'_id', Skey}) of
        #{error := no_entry} ->    % Записи о параметрах
            {error, no_entry};
        System ->
            {ok, System}
    end.


-define(PROTECT, [<<"id">>, <<"_id">>, <<"imei">>, <<"date">>, <<"premium">>, <<"groups">>, <<"lock">>]).

% Патч разрешен только для одного поля за раз
patch(Entity, _Query = #{skey := Skey}, _Options) ->
    Document = maps:without(?PROTECT, Entity),
    % Res = navidb:set(accounts, {username, Username}, Doc),
    Res = navidb:set(systems, Skey, Document),
    Result = #{answer => erlang:list_to_binary(io_lib:format("~p", [Res]))},
    {ok, Result}.
    % % Список зарещенных полей
    % case lists:member(Field, ?PROTECT) of
    %     true ->
    %         {error, <<"Protected">>};
    %     false ->
    %         % TODO: binary_to_atom - это какашка. Сборщик мусора такое не удаляет.
    %         Document = maps:from_list([{erlang:binary_to_atom(Field, latin1), navidb_mongodb:json_to_bson(Value)}]),
    %         Res2 = navidb:set(systems, Skey, Document),
    %         % TODO: Что это вообще за ужос?
    %         Result = #{answer => erlang:list_to_binary(io_lib:format("~p", [Res2]))},
    %         {ok, Result}
    % end;

% patch(_Entity, _Query, _Options) ->
%     {error, enoent}.
