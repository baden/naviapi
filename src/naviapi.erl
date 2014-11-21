%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%% @copyright Denis Batrak
%% @author Batrak Denis <baden.i.ua@gmail.com>
%% @version {@vsn}, {@date} {@time}
%% @doc ErlNaviCC public API
%% @end
%%%-------------------------------------------------------------------
-module(naviapi).

%% API
-export([start/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    application:ensure_all_started(naviapi).


%%====================================================================
%% Internal functions
%%====================================================================