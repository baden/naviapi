%% -*- coding: utf-8 -*-
-module(naviapi_systems_command).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            get/2,
            post/3,
            patch/3,
            delete/2
        ]).

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => true}}.  % Для доступа к ресурсу требуется авторизация

% Получить информацию по всем наблюдаемым системам
% TODO: Не самое удачное решение повторно загружать запись авторизованного пользователя. Но пока сойдет.
get(_Query = #{skey := Skey}, _Options) ->
    Document = case navidb:get(command, Skey) of
        {ok, Command} ->
            #{<<"command">> => Command};
        _ ->
            #{}
    end,
    {ok, Document}.

% sub
post(Entity, _Query = #{skey := Skey, sub := <<"queue">>}, _Options) ->
        % TODO: Это не должно быть тут. Это нужно перенести в rest_resource
        Document = maps:from_list(Entity),
        navidb:update(params, Skey, #{<<"$set">> => #{<<"queue">> => Document}}),
        ok.

% Патч разрешен только для одного поля за раз
patch([{<<"queue">>, Value}], _Query = #{skey := Skey}, _Options) ->
    % TODO: Это не должно быть тут. Это нужно перенести в rest_resource
    Document = maps:from_list(Value),
    Res2 = navidb:set(params, Skey, #{queue => Document}),

    % TODO: Вообще-то тут скорее нужна очередь команд или set
    Command = <<"CONFIGUP\r\n">>,
    navidb:set(command, Skey, Command),

    Result = #{answer => erlang:list_to_binary(io_lib:format("~p", [Res2]))},
    {ok, Result};

patch(_Entity, _Query, _Options) ->
    {error, enoent}.

delete(_Query = #{skey := Skey}, _Options) ->
    navidb:update(params, {'_id', Skey}, #{<<"$unset">> => #{<<"queue">> => <<"">>}}),
    ok.
