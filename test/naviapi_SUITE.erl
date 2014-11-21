-module(naviapi_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [
    {group, noauth},
    {group, auth}
].

groups() ->
    [
        {noauth, [parallel], [info, register]},
        {auth, [parallel], [account, account2, password]}
    ].

init_per_suite(Config) ->
    helper:start(Config).

end_per_suite(Config) ->
    helper:stop(Config).

% init_per_group(noauth, Config) ->
%     Config;
%
% init_per_group(auth, Config) ->
%     % helper:auth(Config).
%     Config.
%
% end_per_group(noauth, Config) ->
%     Config;
%
% end_per_group(auth, Config) ->
%     Config.

init_per_testcase(_Case, Config) ->
    GropProps = ?config(tc_group_properties, Config),
    case ?config(name, GropProps) of
        auth -> helper:auth(Config);
        _ -> Config
    end.

end_per_testcase(_Case, Config) ->
    GropProps = ?config(tc_group_properties, Config),
    case ?config(name, GropProps) of
        auth -> helper:clean(Config);
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

    Username2 = helper:random_string(),

    % Попытка создать группу, которая уже существует
    % {409, _, #{<<"errors">> := [Error1]}} = helper:post(Config, "/register", #{
    %     grant_type => <<"password">>,
    %     title      => Title,
    %     username   => Username2,
    %     password   => Password,
    %     groupname  => Groupname,
    %     grouppassword  => Grouppassword,
    %     newgroup   => true
    % }),
    % #{<<"resource">> := <<"Group">>, <<"code">>     := <<"exist">>} = Error1,
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
    ok.
