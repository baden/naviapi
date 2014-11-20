%% -*- coding: utf-8 -*-
-module(naviapi_geos_hours).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            get/2
        ]).

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => true}}.   % Для доступа к ресурсу требуется авторизация

get(_Query = #{skey := Skey, <<"from">> := From, <<"to">> := To}, _Options) ->
    % TODO: Не самое элегантное решение
    Hours = navidb:get_gps_hours(Skey, binary_to_integer(From), binary_to_integer(To)),

    Result = #{
        system => Skey,
        from   => From,
        to     => To,
        hours  => Hours
    },
    {ok, Result}.
