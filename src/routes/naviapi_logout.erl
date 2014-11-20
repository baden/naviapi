%% -*- coding: utf-8 -*-
-module(naviapi_logout).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
            init/2,
            options/2,
            rest_init/2,
            allowed_methods/2
        ]).

-export([
            content_types_provided/2,
            charsets_provided/2
        ]).

-export([
            process_get/2
        ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

options(Req, State) ->
    Req1 = api_auth:cors(Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"OPTIONS, GET">>, Req1),
    {ok, Req2, State}.

rest_init(Req, _Opts) ->
    {ok, Req, undefined_state}.

content_types_provided(Req, State) ->
    Req1 = api_auth:cors(Req),
    {[
        {{<<"application">>, <<"json">>, []}, process_get},
        {{<<"text">>, <<"html">>, []}, process_get}
    ], Req1, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>,<<"GET">>], Req, State}.

process_get(Req, State) ->
    Params = [{logout, true}],
    Req1 = cowboy_req:set_resp_cookie(<<"access_token">>, <<"">>, [{path, <<"/">>}], Req),
    {ok, Reply} = cowboy_req:reply(200, [], jsxn:encode(Params, [space, {indent, 4}]), Req1),
    {halt, Reply, State}.
