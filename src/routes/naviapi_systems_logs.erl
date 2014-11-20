%% -*- coding: utf-8 -*-
-module(naviapi_systems_logs).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            get/2,
            delete/2
        ]).

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => true}}.  % Для доступа к ресурсу требуется авторизация

% Получить информацию по всем наблюдаемым системам
get(Query = #{skey := Skey}, _Options) ->
    Skip  = binary_to_integer(maps:get(<<"skip">>, Query, <<"100000000000">>)),
    Count = binary_to_integer(maps:get(<<"count">>, Query, <<"20">>)),

    Docs = navidb:get_logs(Skey, Count, Skip),
    {ok, Docs}.

delete(Query = #{skey := Skey}, _Options = #{username := Username}) ->
    % Удалять может только член группы администраторов
    case api_auth:is_admin(Username) of
        false ->
            {error, <<"NOADMIN">>};
        true ->
            From    = binary_to_integer(maps:get(<<"from">>, Query, <<"0">>)),
            To      = binary_to_integer(maps:get(<<"to">>, Query, <<"0">>)),

            case maps:get_value(logid, Query, undefined) of
                undefined ->
                    navidb:remove(logs, #{'system' => Skey, 'dt' => #{'$gte' => From, '$lte' => To}}),
                    ok;

                Lkey ->
                    Key = base64:decode(Lkey),
                    navidb:remove(logs, #{'_id' => {Key}}),
                    ok
            end
    end.
