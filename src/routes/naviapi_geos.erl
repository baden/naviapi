%% -*- coding: utf-8 -*-
-module(naviapi_geos).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            get/2,
            delete/2
        ]).

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => true}}.  % Для доступа к ресурсу требуется авторизация

get(_Query = #{skey := Skey, <<"from">> := From, <<"to">> := To}, _Options) ->
    % TODO: Тут ответ должен быть бинарным (arraybuffer)
    % Content-Type = 'application/octet-stream; charset=binary'
    {ok, Data} = navidb:get_geos(Skey, binary_to_integer(From), binary_to_integer(To)),
    {binary, Data}.

delete(_Query = #{skey := Skey, <<"from">> := From, <<"to">> := To}, _Options = #{username := Username}) ->
    case naviapi_rest:is_admin(Username) of
        false ->
            lager:warning("Permissions denied for ~p", [Username]),
            {error, <<"NOADMIN">>};
        true ->
            Selector = #{
                <<"system">> => Skey,
                <<"hour">> => #{
                    <<"$gte">> => binary_to_integer(From),
                    <<"$lte">> => binary_to_integer(To)
                }
            },
            navidb:remove(gps, Selector, {flush, {gps, Skey}}),
            ok
    end.
