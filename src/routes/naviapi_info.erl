%% -*- coding: utf-8 -*-
-module(naviapi_info).

-export([init/2]).
-export([options/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req1),
    {ok, Req2, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, hello_to_html},
        {<<"application/json">>, hello_to_json}
    ], Req, State}.

hello_to_html(Req, State) ->
    Body = <<"<html><head><meta charset=\"utf-8\"><title>API info</title></head><body><p>REST methods: GET</p></body></html>">>,
    {Body, Req, State}.

hello_to_json(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),

    Result = [
        {info, <<"TBD">>}
    ],

    Body = jsxn:encode(Result),

    {Body, Req1, State}.
