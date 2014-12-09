%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%% @copyright Denis Batrak
%% @author Batrak Denis <baden.i.ua@gmail.com>
%% @version {@vsn}, {@date} {@time}
%% @doc ErlNaviCC public API
%% @end
%%%-------------------------------------------------------------------
-module(naviapi_app).

-behaviour(application).

-define(APP, naviapi).

%% Application callbacks
-export([start_phase/3, start/2, stop/1]).

% -export([config/0, config/1, config/2).
        %  start/0, a_start/2]).

-define(APIVERSION, "1.0").

%%%===================================================================
%%% Convenience Functions
%%%===================================================================


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    oauth2_ets_backend:start(),
    naviapi_sup:start_link().

stop(_State) ->
    oauth2_ets_backend:stop(),
    ok.

start_phase(listen, _Type, _Args) ->
    Opts = #{},
    Dispatch = [
        {'_', [
            {"/" ++ ?APIVERSION ++ "/auth",     naviapi_auth, Opts},
            {"/" ++ ?APIVERSION ++ "/register", naviapi_register, Opts},
            {"/" ++ ?APIVERSION ++ "/logout",   naviapi_logout, Opts},
            {"/" ++ ?APIVERSION ++ "/info",     naviapi_info, Opts},
            {"/" ++ ?APIVERSION ++ "/account/systems[/:skey]",  naviapi_account_systems, Opts},
            {"/" ++ ?APIVERSION ++ "/account[/:akey]",          naviapi_account, Opts},
            {"/" ++ ?APIVERSION ++ "/systems/[:skey]",          naviapi_systems, Opts},
            {"/" ++ ?APIVERSION ++ "/systems/[:skey]/params",   naviapi_systems_params, Opts},
            {"/" ++ ?APIVERSION ++ "/systems/[:skey]/command",  naviapi_systems_command, Opts},
            {"/" ++ ?APIVERSION ++ "/params/[:skey]",           naviapi_systems_params, Opts},
            {"/" ++ ?APIVERSION ++ "/systems/[:skey]/logs[/:logid]", naviapi_systems_logs, Opts},
            {"/" ++ ?APIVERSION ++ "/geos/[:skey]/hours",   naviapi_geos_hours, Opts},
            {"/" ++ ?APIVERSION ++ "/geos/[:skey]",         naviapi_geos, Opts},
            {"/" ++ ?APIVERSION ++ "/gsmcell",         naviapi_gsmcell, Opts}
        ]}
    ],

    cowboy:start_http(?APP, config(acceptors, 100),
                      [{port, config(port)}],
                      [{env,
                        [{dispatch, cowboy_router:compile(Dispatch)}]}]),
    ok.

% %%%===================================================================
% %%% Internal functions
% %%%===================================================================

config(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(Key) ->
    case application:get_env(?APP, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.
