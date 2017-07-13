%% -*- coding: utf-8 -*-
-module(naviapi_account_systems).
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
get(_Query, _Options = #{username := Username}) ->
    #{<<"skeys">> := Skeys} = navidb:get(accounts, {username, Username}),
    Systems = navidb:get(systems, Skeys),
    {ok, Systems}.

% TODO: Этот метод стоит убрать из API. Это как-то совсем не по REST-канонам
% Добавить системы в список наблюдения
post(#{<<"cmd">> := <<"add">>, <<"imeis">> := Imeis}, _Query, _Options = #{username := Username}) ->
    #{<<"skeys">> := Skeys} = navidb:get(accounts, {username, Username}),

    Result = lists:foldl(
        fun(Imei, Res) ->
            % Проверим, возможно пользователь уже имет трекер в списке наблюдения
            Skey = base64:encode(Imei),
            [case lists:member(Skey, Skeys) of
                true ->
                    #{
                        result => <<"already">>,
                        system => null
                    };
                false ->
                    % Получим запись о трекере
                    % case navidb:get(systems, #{'_id' => Skey}) of
                    case navidb:get(systems, Skey) of
                        #{error := no_entry} ->    % Записи о системе еще нет.
                            #{
                                result => <<"notfound">>,
                                system => null
                            };
                        System ->
                            navidb:update(accounts, {username, Username}, #{<<"$addToSet">> => {skeys, Skey}}),
                            #{
                                result => <<"added">>,
                                system => System
                            }
                    end
            end | Res]
        end,
        [],
        Imeis
    ),

    {ok, Result}.

patch(_Entity, _Query, _Options) ->
    Result = #{answer => <<"TBD">>},
    {ok, Result}.

delete(_Query = #{skey := Skey}, _Options = #{username := Username}) ->
    navidb:update(accounts, {username, Username}, #{'$pull' => #{skeys => Skey}}),
    ok.
