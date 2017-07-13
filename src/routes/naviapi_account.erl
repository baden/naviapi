%% -*- coding: utf-8 -*-
-module(naviapi_account).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            get/2,
            patch/3,
            put/3
        ]).

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => true}}.  % Для доступа к ресурсу требуется авторизация

get(_Query, _Options = #{username := Username}) ->
    Account = navidb:get(accounts, {username, Username}, {filter, ['_id', 'password']}),
    {ok, Account}.

-define(PROTECT, [<<"id">>, <<"_id">>, <<"username">>, <<"date">>, <<"premium">>, <<"groups">>]).

% Патч разрешен только для одного поля за раз
patch(Entity, _Query, _Options = #{username := Username}) ->
    Doc = maps:without(?PROTECT, Entity),
    Res = navidb:set(accounts, {username, Username}, Doc),
    Result = #{answer => erlang:list_to_binary(io_lib:format("~p", [Res]))},
    {ok, Result}.

% @doc Изменение пароля
put(#{<<"password">> := Password, <<"old_password">> := OldPassword}, _Query, _Options = #{username := Username}) ->
    #{<<"password">> := CurrentPassword} = navidb:get(accounts, {username, Username}),
    case CurrentPassword of
        OldPassword ->
            navidb:set(accounts, {username, Username}, #{<<"password">> => Password}),
            {ok, #{
                message      => <<"success">>
            }};
        _ ->
            {422, #{
                message => <<"Validation Failed">>,
                errors  => #{
                    resource  => <<"account">>,
                    field     => <<"old_password">>,
                    code      => <<"not_match">>
                }
            }}
    end.
