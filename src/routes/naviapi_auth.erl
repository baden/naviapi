%% -*- coding: utf-8 -*-
-module(naviapi_auth).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            post/3
        ]).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => false}}. % Клиент еще не авторизован

post(Entity = #{<<"grant_type">> := <<"password">>}, _Query, _Options) ->
    process_password_grant(Entity);

post(_Entity, _Query, _Options) ->
    {error, noallwd}.

%%%===================================================================
%%% Grant type handlers
%%%===================================================================

process_password_grant(Params = #{<<"username">> := Username, <<"password">> := Password}) ->
    % Username = proplists:get_value(<<"username">>, Params),
    % Password = proplists:get_value(<<"password">>, Params),
    Scope    = maps:get(<<"scope">>, Params, <<"">>),

    case navidb:get(accounts, {username, Username}) of
        #{error := no_entry} ->
            % access_denied
            emit_response({error, access_denied});
        #{<<"password">> := DbPassword} ->
            % Возможно пользователь уже есть в памяти
            case oauth2_ets_backend:authenticate_username_password(Username, Password, []) of
                {error, notfound} ->            % Нет, пользователя еще нет, подгрузим его
                    oauth2_ets_backend:add_user(Username, DbPassword);
                _ -> ok
            end,

            Auth = oauth2:authorize_password(Username, Password, Scope, []),
            issue_token(Auth);
        _ ->  % Обычно этого не бывает, но если нет записи password, то вызов крашится
          emit_response({error, access_denied})
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

issue_token({ok, Auth}) ->
    emit_response(oauth2:issue_token(Auth, []));
issue_token(Error) ->
    emit_response(Error).

emit_response(AuthResult) ->
    case AuthResult of
        {error, Reason} ->
            {error, jsxn:encode([{error, to_binary(Reason)}], [space, {indent, 4}])};
        Response ->
            Proplist = oauth2_response:to_proplist(Response),
            AccessToken = proplists:get_value(<<"access_token">>, Proplist),
            % io:format("AccessTocken=~p~n", [AccessToken]),
            {
                ok,
                to_json_term(oauth2_response:to_proplist(Response), []),
                fun (Req) ->
                    MaxAge = 365 * 24*60*60, % 365 дней
                    cowboy_req:set_resp_cookie(<<"access_token">>, AccessToken, [{path, <<"/">>}, {max_age, MaxAge}], Req)
                end
            }
    end.

to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

to_json_term([], Acc) ->
    Acc;
to_json_term([{H, {HK, HV}} |  T], Acc) ->
    to_json_term(T, [{H, <<"{",HK/binary,",",HV/binary,"}">>} | Acc]);
to_json_term([H | T], Acc) ->
    to_json_term(T, [H | Acc]).
