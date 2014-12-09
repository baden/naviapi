-module(naviapi_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(REPEAT, 1).
% -define(REPEAT, 20).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [
    {group, noauth},
    {group, auth},
    {group, auth_tracker}
].

groups() ->
    [
        {noauth, [parallel, shuffle, {repeat, ?REPEAT}], [info, register, gsmcell]},
        {auth,   [parallel, shuffle, {repeat, ?REPEAT}], [account, account2, password, account_systems]},
        {auth_tracker, [parallel, shuffle, {repeat, ?REPEAT}], [geos]}
    ].

init_per_suite(Config) ->
    helper:start(Config).

end_per_suite(Config) ->
    helper:stop(Config),
    ok.

init_per_group(_G, Config) ->
    Config.

end_per_group(_G, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    GropProps = ?config(tc_group_properties, Config),
    case ?config(name, GropProps) of
        auth -> helper:auth(Config);
        auth_tracker -> helper:tracker(create, helper:auth(Config));
        _ -> Config
    end.

end_per_testcase(_Case, Config) ->
    GropProps = ?config(tc_group_properties, Config),
    case ?config(name, GropProps) of
        auth -> helper:clean(Config);
        auth_tracker -> helper:tracker(clean, helper:clean(Config));
        _ -> Config
    end.

info(Config) ->
    % Out of REST API
    {200, _, _} = helper:get(Config, "/info"),
    ok.

register(Config) ->
    Title = helper:random_string(),
    Username = helper:random_string(),
    Password = helper:random_string(),
    Email    = helper:random_string(),
    Groupname = helper:random_string(),
    Grouppassword = helper:random_string(),

    % Pure user
    {200, _, _} = helper:post(Config, "/register", #{
        grant_type => <<"password">>,
        % title      => Title,  % Должен установиться равным Username
        username   => Username,
        password   => Password,
        email      => Email
    }),
    timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать
    Account = navidb:get(accounts, #{username => Username}),
    ?assertMatch(
        #{
            date     := _,
            email    := Email,
            groups   := [],
            id       := _,
            password := Password,
            skeys    := [],
            title    := Username,
            username := Username
        },
        Account
    ),
    navidb:remove(accounts, #{username => Username}),

    % With unexisting group
    {404, _, _} = helper:post(Config, "/register", #{
        grant_type => <<"password">>,
        title      => Title,
        username   => Username,
        password   => Password,
        groupname  => Groupname,
        grouppassword  => Grouppassword,
        newgroup   => false
    }),

    % Create group
    {200, _, _} = helper:post(Config, "/register", #{
        grant_type => <<"password">>,
        title      => Title,
        username   => Username,
        password   => Password,
        groupname  => Groupname,
        grouppassword  => Grouppassword,
        newgroup   => true
    }),
    timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать

    Username2 = helper:random_string(),

    % Попытка создать группу, которая уже существует
    ?assertMatch(
        {409, _, #{
            <<"errors">> := [#{
                <<"resource">> := <<"Group">>,
                <<"code">>     := <<"exist">>
            }]}
        },
        helper:post(Config, "/register", #{
            grant_type => <<"password">>,
            title      => Title,
            username   => Username2,
            password   => Password,
            groupname  => Groupname,
            grouppassword  => Grouppassword,
            newgroup   => true
        })
    ),

    % Попытка присоединиться к группе, не зная проверочного слова
    ?assertMatch(
        {409, _, #{
            <<"errors">> := [#{
                <<"resource">> := <<"Group">>,
                <<"code">>     := <<"wrongpassword">>
            }]}
        },
        helper:post(Config, "/register", #{
            grant_type => <<"password">>,
            title      => Title,
            username   => Username2,
            password   => Password,
            groupname  => Groupname,
            grouppassword  => helper:random_string(),
            newgroup   => false
        })
    ),

    % А вот теперь должно получиться
    {200, _, _} = helper:post(Config, "/register", #{
        grant_type => <<"password">>,
        title      => Title,
        username   => Username2,
        password   => Password,
        groupname  => Groupname,
        grouppassword  => Grouppassword,
        newgroup   => false
    }),
    timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать

    % Попытка повторно создать пользователя
    ?assertMatch(
        {409, _, #{
            <<"errors">> := [#{
                <<"resource">> := <<"GroupMember">>,
                <<"code">>     := <<"exist">>
            }]}
        },
        helper:post(Config, "/register", #{
            grant_type => <<"password">>,
            title      => Title,
            username   => Username2,
            password   => Password,
            groupname  => Groupname,
            grouppassword  => Grouppassword,
            newgroup   => false
        })
    ),

    ?assertMatch(
        #{
            members := [Username, Username2]
        },
        navidb:get(groups, {groupname, Groupname})
    ),

    navidb:remove(accounts, {username, Username}),
    navidb:remove(accounts, {username, Username2}),
    navidb:remove(groups, {groupname, Groupname}),
    ok.

account(Config) ->
    Username = ?config(username, Config),
    {200, _, AccountBody} = helper:get(Config, "/account"),
    ?assertMatch(#{<<"username">> := Username}, AccountBody),

    {200, LogoutHeaders, _} = helper:get(Config, "/logout"),
    Cookie = proplists:get_value(<<"set-cookie">>, LogoutHeaders),
    {ok, RE_Cookie} = re:compile("access_token=;"),
    {match,[_]} = re:run(Cookie, RE_Cookie, []),
    ok.

account2(Config) ->
    Username = ?config(username, Config),

    % GET before
    ?assertMatch(
        {200, _, #{<<"username">> := Username}},
        helper:get(Config, "/account")
    ),

    RandomValue = helper:random_string(),
    % PATCH
    {200, _, _} = helper:patch(Config, "/account", #{<<"foo">> => RandomValue}),
    timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать

    % GET after
    ?assertMatch(
        {200, _, #{<<"username">> := Username, <<"foo">> := RandomValue}},
        helper:get(Config, "/account")
    ),
    ok.

password(Config) ->
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    ?assertMatch(
        {200, _, #{<<"username">> := Username}},
        helper:get(Config, "/account")
    ),
    NewPassword = helper:random_string(),
    WrongOldPassword = helper:random_string(),

    % Сначала мы ошибемся во вводе старого пароля
    Payload1 = #{
        old_password => WrongOldPassword,
        password     => NewPassword
    },
    {422, _, RespBody1} = helper:put(Config, "/account", Payload1),
    ?assertMatch(
        #{
            <<"errors">> := #{
                <<"resource">> := <<"account">>,
                <<"field">>    := <<"old_password">>,
                <<"code">>     := <<"not_match">>
            }
        },
        RespBody1
    ),

    % Теперь введем его правильно
    Payload2 = #{
        old_password => Password,
        password     => NewPassword
    },
    {200, _, _} = helper:put(Config, "/account", Payload2),
    % timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать
    % TODO: тут бы попробовать залогиниться под новым паролем
    ok.

account_systems(Config) ->
    % Username = ?config(username, Config),
    ?assertMatch(
        {200, _, #{<<"skeys">> := []}},
        helper:get(Config, "/account")
    ),

    % Попробуем добавить несуществующий трекер
    Imei1 = helper:random_string(),

    ?assertMatch(
        {200, _, [#{
            <<"result">> := <<"notfound">>,
            <<"system">> := null
        }]},
        helper:post(Config, "/account/systems", #{<<"cmd">> => <<"add">>, imeis => [Imei1]})
    ),

    Skey = base64:encode(Imei1),
    #{title := Title1, date := Date1} = navidb:get(system, Skey, cached), % Это создаст систему
    timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать
    % ct:pal("System1 = ~p", [System1]),

    % В этот раз должно быть все хорошо
    {200, _, [#{<<"result">> := <<"added">>, <<"system">> := System2}]} =
        helper:post(Config, "/account/systems", #{<<"cmd">> => <<"add">>, imeis => [Imei1]}),
    % ct:pal("System2 = ~p", [System2]),
    % Проверим парочку полей
    ?assertMatch(
        #{<<"id">> := Skey, <<"imei">> := Imei1, <<"title">> := Title1, <<"date">> := Date1},
        System2
    ),
    timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать
    % Попробуем повторно добавить трекер
    ?assertMatch(
        {200, _, [#{
            <<"result">> := <<"already">>,
            <<"system">> := null
        }]},
        helper:post(Config, "/account/systems", #{<<"cmd">> => <<"add">>, imeis => [Imei1]})
    ),
    timer:sleep(100), % Это нужно чтобы пулл базы данных успел записать
    % Удалим трекер
    {204, _, _} = helper:delete(Config, "/account/systems/" ++ binary_to_list(Skey)),
    navidb:remove(systems, {id, Skey}),
    ok.

geos(Config) ->
    % Imei = ?config(imei, Config),
    Skey = ?config(skey, Config),


    ok = navidb_gpsdb:save(Skey, 10, <<"fake-data-01">>),
    timer:sleep(10), % Это нужно чтобы пулл базы данных успел записать (скорее всего не нужно, так как данные кешируются в памяти)

    %
    {200, Headers, <<"fake-data-01">>} = helper:get(Config, "/geos/" ++ helper:escape_uri(Skey), #{from => 9, to => 11}),
    <<"application/octet-stream; charset=binary">> = proplists:get_value(<<"content-type">>, Headers),
    % ct:pal("Result = ~p", [Result]),
    ok.

gsmcell(Config) ->
    MCC = 255,
    MNC = 3,
    LAC = 16#b7e8,
    CID = 16#24F5,
    {200, _, Response} = helper:get(Config, "/gsmcell", #{mcc => MCC, mnc => MNC, lac => LAC, cid => CID}),
    ct:log("Response = ~p", [Response]),
    ok.
