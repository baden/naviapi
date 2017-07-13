%% -*- coding: utf-8 -*-
-module(naviapi_systems_params).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            get/2,
            post/3,
            patch/3,
            delete/2
        ]).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => true}}.  % Для доступа к ресурсу требуется авторизация

% Получить информацию по всем наблюдаемым системам
% TODO: Не самое удачное решение повторно загружать запись авторизованного пользователя. Но пока сойдет.
get(_Query = #{skey := Skey}, _Options) ->
    case navidb:get(params, {'_id', Skey}) of
        #{error := no_entry} ->    % Записи о параметрах
            {ok, #{<<"data">> => #{}, id => Skey}}; % Вернем пустые данные
        Document = #{<<"data">> := Data} ->
            % Я лоханулся. Требуется отфильтровать двойные кавычки
            Filtered = maps:fold(
                fun(Name, #{<<"type">> := Type, <<"value">> := Value, <<"default">> := Default}, Acc) ->
                    maps:put(
                        type_to_repr(Name),
                        #{
                            <<"type">>    => Type,
                            <<"value">>   => remquotes(Value),
                            <<"default">> => remquotes(Default)
                        },
                        Acc
                    )
                end,
                #{},
                Data
            ),
            % Document2 = #{data => Filtered, queue => Queue, id => Id},
            Document2 = Document#{<<"data">> := Filtered},

            {ok, Document2}
    end.

remquotes(In) ->
    binary:replace(In, <<"\"">>, <<"">>, [global]).

type_to_repr(Label) ->
    binary:replace(atom_to_binary(Label, utf8), <<$#>>, <<$.>>, [global]).

% sub

post(Document, _Query = #{skey := Skey, sub := <<"queue">>}, _Options) ->
    navidb:update(params, Skey, #{<<"$set">> => #{<<"queue">> => Document}}),
    ok.

% Патч разрешен только для одного поля за раз
patch(#{<<"queue">> := Queue}, _Query = #{skey := Skey}, _Options) ->
    Res2 = navidb:set(params, Skey, #{<<"queue">> => Queue}),

    % TODO: Вообще-то тут скорее нужна очередь команд или set
    Command = <<"CONFIGUP\r\n">>,
    navidb:set(command, Skey, Command),

    Result = #{answer => erlang:list_to_binary(io_lib:format("~p", [Res2]))},
    {ok, Result};

patch(_Entity, _Query, _Options) ->
    {error, enoent}.

delete(#{skey := Skey}, _Options) ->
    navidb:update(params, {'_id', Skey}, #{<<"$unset">> => #{<<"queue">> => <<"">>}}),
    ok.
