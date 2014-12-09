%% -*- coding: utf-8 -*-
-module(naviapi_gsmcell).
-author('Denis Batrak <baden.i.ua@gmail.com>').

-export([
    init/2,
    get/2
    % patch/3
]).

% Suppress unused warning
% -export([openCellID/4, google/4, yandex/4, mozLocation/4]).
% -compile([{nowarn_unused_function, [
%     {openCellID, 4}, {google, 4}, {yandex, 4}, {mozLocation, 4}
% ]}]).

init(Req, Opts) ->
    {naviapi_rest, Req, Opts#{auth => false}}.  % Для доступа к ресурсу не требуется авторизация

get(Query, _Options) ->
    MCC = binary_to_integer(maps:get(<<"mcc">>, Query)),
    MNC = binary_to_integer(maps:get(<<"mnc">>, Query)),
    LAC = binary_to_integer(maps:get(<<"lac">>, Query)),
    CID = binary_to_integer(maps:get(<<"cid">>, Query)),

    Start = unixtime1000x(),

    % TODO! Async requests
    Self_PID = self(),
    % OpenCellID = openCellID(MCC, MNC, LAC, CID),
    _OpenCellID_PID = spawn(fun() -> client(Self_PID, openCellID, MCC, MNC, LAC, CID) end),
    % Google = google(MCC, MNC, LAC, CID),
    _Google_PID = spawn(fun() -> client(Self_PID, google, MCC, MNC, LAC, CID) end),
    % Yandex = yandex(MCC, MNC, LAC, CID),
    _Yandex_PID = spawn(fun() -> client(Self_PID, yandex, MCC, MNC, LAC, CID) end),
    % MozLocation = mozLocation(MCC, MNC, LAC, CID, gsm),
    _MozLocation_PID = spawn(fun() -> client(Self_PID, mozLocation, MCC, MNC, LAC, CID) end),

    {OpenCellID, Google, Yandex, MozLocation} = wait(),

    Stop = unixtime1000x(),

    Result = #{
        params => #{
            mcc => MCC,
            mnc => MNC,
            lac => LAC,
            cid => CID
        },
        openCellID => OpenCellID,
        google => Google,
        yandex => Yandex,
        moz_location => MozLocation,
        info => #{
            start => Start,
            stop => Stop,
            duration => Stop - Start
        }
    },
    {ok, Result}.

wait() ->
    wait({undefined, undefined, undefined, undefined}).

wait({OpenCellID, Google, Yandex, MozLocation}) when OpenCellID == undefined orelse Google == undefined orelse Yandex == undefined orelse MozLocation == undefined ->
    % ct:pal("wait(~p, ~p, ~p, ~p)", [OpenCellID, Google, Yandex, MozLocation]),
    receive
        {done, {openCellID, Result}} ->
            wait({Result, Google, Yandex, MozLocation});
        {done, {google, Result}} ->
            wait({OpenCellID, Result, Yandex, MozLocation});
        {done, {yandex, Result}} ->
            wait({OpenCellID, Google, Result, MozLocation});
        {done, {mozLocation, Result}} ->
            wait({OpenCellID, Google, Yandex, Result});
        {error, {openCellID, Class, Reason, StackTrace}} ->
            ErrorResult = #{
                error => #{
                    class => list_to_binary(io_lib:format("~p", [Class])),
                    reason => list_to_binary(io_lib:format("~p", [Reason])),
                    stack => list_to_binary(io_lib:format("~p", [StackTrace]))
                }
            },
            wait({ErrorResult, Google, Yandex, MozLocation});
        {error, {google, Class, Reason, StackTrace}} ->
            ErrorResult = #{
                error => #{
                    class => list_to_binary(io_lib:format("~p", [Class])),
                    reason => list_to_binary(io_lib:format("~p", [Reason])),
                    stack => list_to_binary(io_lib:format("~p", [StackTrace]))
                }
            },
            wait({OpenCellID, ErrorResult, Yandex, MozLocation});
        {error, {yandex, Class, Reason, StackTrace}} ->
            ErrorResult = #{
                error => #{
                    class => list_to_binary(io_lib:format("~p", [Class])),
                    reason => list_to_binary(io_lib:format("~p", [Reason])),
                    stack => list_to_binary(io_lib:format("~p", [StackTrace]))
                }
            },
            wait({OpenCellID, Google, ErrorResult, MozLocation});
        {error, {mozLocation, Class, Reason, StackTrace}} ->
            ErrorResult = #{
                error => #{
                    class => list_to_binary(io_lib:format("~p", [Class])),
                    reason => list_to_binary(io_lib:format("~p", [Reason])),
                    stack => list_to_binary(io_lib:format("~p", [StackTrace]))
                }
            },
            wait({OpenCellID, Google, Yandex, ErrorResult})
    after 6000 ->
        % ct:pal("wait: Timeout"),
        {OpenCellID, Google, Yandex, MozLocation}
    end;
% All Done
wait({OpenCellID, Google, Yandex, MozLocation}) ->
    % ct:pal("wait:Done"),
    {OpenCellID, Google, Yandex, MozLocation}.

client(Self_PID, Method, MCC, MNC, LAC, CID) ->
    % ct:pal("Start client ~p", [Method]),
    try client(Method, MCC, MNC, LAC, CID) of
        Any ->
            % ct:pal("Done client ~p Result = ~p", [Method, Any]),
            Self_PID ! {done, {Method, Any}}
    catch
        Class:Reason ->
            % ct:pal("Error client ~p Result = ~p:~p", [Method, Class, Reason]),
            Self_PID ! {error, {Method, Class, Reason, erlang:get_stacktrace()}}
    end.

client(openCellID, MCC, MNC, LAC, CID) -> openCellID(MCC, MNC, LAC, CID);
client(google, MCC, MNC, LAC, CID)     -> google(MCC, MNC, LAC, CID);
client(yandex, MCC, MNC, LAC, CID)     -> yandex(MCC, MNC, LAC, CID);
client(mozLocation, MCC, MNC, LAC, CID) -> mozLocation(MCC, MNC, LAC, CID).

openCellID(MCC, MNC, LAC, CID) ->
    % ct:pal("openCellID", []),
    Start = unixtime1000x(),

    OpenCellIDApiKey = "96989eae-4fd0-4cc5-ad72-770aff066262",

    % http://opencellid.org/cell/get?key=#{openCellIDApiKey}&mnc=99&mcc=250&lac=13952&cellid=49983

    Url = "http://opencellid.org/cell/get?"
        "key="  ++ OpenCellIDApiKey ++
        "&mnc=" ++ integer_to_list(MNC) ++
        "&mcc=" ++ integer_to_list(MCC) ++
        "&lac=" ++ integer_to_list(LAC) ++
        "&cellid=" ++ integer_to_list(CID),
    Header = [],

    RpcResult = httpc:request(get, {Url, Header}, [{timeout, 5000}, {connect_timeout, 1000}], []),
    % ct:pal("openCellID: RpcResult = ~p", [RpcResult]),
    {ok, {{_, 200, _}, _, Html}} = RpcResult,
    % ct:pal("openCellID: Html = ~p", [Html]),

    Result = case re:run(Html, "err\\s+info=\"([^\"]+)\"\\s+code=\"([^\"]+)\"", [{capture, all_but_first, list}]) of
        {match, [ErrInfo, Code]} ->
            #{
                error_info => list_to_binary(ErrInfo),
                code       => list_to_integer(Code),
                raw        => list_to_binary(Html)
            };
        _ ->
            % <?xml version=\"1.0\" encoding=\"UTF-8\"?>\n
            % <rsp stat=\"ok\">\n
            % <cell
            %   lat=\"48.50112255302201\"
            %   lon=\"34.62752637197796\"
            %   mcc=\"255\" mnc=\"3\" lac=\"47080\" cellid=\"9461\"
            %   averageSignalStrength=\"0\" range=\"-1\" samples=\"364\" changeable=\"1\" radio=\"GSM\" />
            {match, [LatitudeS]} = re:run(Html, "\\slat=\"([+\\-\\d\\.]+)\"", [{capture, all_but_first, list}]),
            {match, [LongitudeS]} = re:run(Html, "\\slon=\"([+\\-\\d\\.]+)\"", [{capture, all_but_first, list}]),
            #{
                latitude  => list_to_float(LatitudeS),
                longitude => list_to_float(LongitudeS)
            }
    end,

    Stop = unixtime1000x(),
    maps:merge(
        Result,
        #{
            info => #{
                start => Start,
                stop => Stop,
                duration => Stop - Start
            }
        }
    ).

google(_MCC, _MNC, LAC, CID) ->
    % ct:pal("google", []),
    Start = unixtime1000x(),
    Url = "http://www.google.com/glm/mmap",
    % Header = [
    %     { "User-Agent", "Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)"}
    % ],
    Header = [],
    Payload = <<
    21:16/big-unsigned-integer, % pack16(21);             // h 21
    0:64/big-unsigned-integer,  % pack64(0);              // q 0
    2:16/big-unsigned-integer,  % packS("ua");            // h len(country)
    "ua",                       % packS("ua");            // 2s country
    13:16/big-unsigned-integer, % packS("Nokia N95 8Gb"); // h len(device)
    "Nokia N95 8Gb",            % packS("Nokia N95 8Gb"); // 13s device
    5:16/big-unsigned-integer,  % packS("1.3.1");         // h len('1.3.1')
    "1.3.1",                    % packS("1.3.1");         // 5s "1.3.1"
    3:16/big-unsigned-integer,  % packS("Web");           // h len('Web')
    "Web",                      % packS("Web");           // 3s "Web"
    27,                         % pack8(27);              // B 27
    0:32/big-unsigned-integer,  % pack32(0);              // i 0
    0:32/big-unsigned-integer,  % pack32(0);              // i 0
    3:32/big-unsigned-integer,  % pack32(3);              // i 3
    0:16/big-unsigned-integer,  % pack16(0);              // h 0
    CID:32/big-unsigned-integer,  % pack32(cid);            // i cid
    LAC:32/big-unsigned-integer,  % pack32(lac);            // i lac
    0:32/big-unsigned-integer,  % pack32(0);              // i 0
    0:32/big-unsigned-integer,  % pack32(0);              // i 0
    0:32/big-unsigned-integer,  % pack32(0);              // i 0
    0:32/big-unsigned-integer   % pack32(0);              // i 0
    >>,
    % {ok, {_, _, Html}} = httpc:request(post, {Url, Header, "application/binary", Payload}, [{relaxed, true}], []),
    RpcResult = httpc:request(post, {Url, Header, "application/binary", Payload}, [{timeout, 5000}, {connect_timeout, 1000}], [{body_format, binary}]),
    % ct:pal("google: RpcResult = ~p", [RpcResult]),
    Result = case RpcResult of
        {ok, {{_, 200, _}, _, Html}} ->
            <<
                _ResA:16/big-unsigned-integer,       % h a
                _ResB,                               % B b
                ErrorCode:32/big-unsigned-integer,  % i errorCode
                Latitude:32/big-unsigned-integer,   % i latitude
                Longitude:32/big-unsigned-integer,  % i longitude
                _ResC:32/big-unsigned-integer,   % i c
                _ResD:32/big-unsigned-integer,   % i d
                _ResE:16/big-unsigned-integer,   % h e
            % _Rest/binary>> = list_to_binary(Html),
            _Rest/binary>> = Html,
            % ct:pal("A = ~p", [A]),
            % ct:pal("B = ~p", [B]),
            % ct:pal("Html = ~p", [Html]),
            #{
                error_code => ErrorCode,
                latitude => Latitude / 1000000.0,
                longitude => Longitude / 1000000.0
            };
        {ErrorResult, {{_, Code, _}, _, _}} = Full ->
            % ct:pal("Full = ~p", [Full]),
            #{
                error => atom_to_binary(ErrorResult, latin1),
                full  => list_to_binary(io_lib:format("~p", [Full])),
                code  => Code
            }
    end,

    Stop = unixtime1000x(),
    maps:merge(Result, #{
        info => #{
            start => Start,
            stop => Stop,
            duration => Stop - Start
        }
    }).

yandex(MCC, MNC, LAC, CID) ->
    % ct:pal("yandex", []),
    Start = unixtime1000x(),
    % http://mobile.maps.yandex.net/cellid_location/?&cellid=49973&operatorid=99&countrycode=250&lac=13955

    Url = "http://mobile.maps.yandex.net/cellid_location/?&"
        "cellid=" ++ integer_to_list(CID) ++
        "&operatorid=" ++ integer_to_list(MNC) ++
        "&countrycode=" ++ integer_to_list(MCC) ++
        "&lac=" ++ integer_to_list(LAC),
    Header = [],

    Result = case httpc:request(get, {Url, Header}, [{timeout, 5000}, {connect_timeout, 1000}], []) of
        {ok, {{_, 200, _}, _, Html}} ->
            % ct:pal("Html = ~p", [Html]),

            {match, [LatitudeS]} = re:run(Html, "\\slatitude=\"([+\\-\\d\\.]+)\"", [{capture, all_but_first, list}]),
            {match, [LongitudeS]} = re:run(Html, "\\slongitude=\"([+\\-\\d\\.]+)\"", [{capture, all_but_first, list}]),
            {match, [NLatitudeS]} = re:run(Html, "\\snlatitude=\"([+\\-\\d\\.]+)\"", [{capture, all_but_first, list}]),
            {match, [NLongitudeS]} = re:run(Html, "\\snlongitude=\"([+\\-\\d\\.]+)\"", [{capture, all_but_first, list}]),
            {match, [Source]} = re:run(Html, "\\ssource=\"([^\".]+)\"", [{capture, all_but_first, list}]),
            #{
                latitude => list_to_float(LatitudeS),
                longitude => list_to_float(LongitudeS),
                nlatitude => list_to_float(NLatitudeS),
                nlongitude => list_to_float(NLongitudeS),
                source => list_to_binary(Source)
            };
        Other ->
            % ct:pal("Request Resp = ~p", [Other]),
            #{error => list_to_binary(io_lib:format("~p", [Other]))}
    end,

    Stop = unixtime1000x(),
    maps:merge(Result,
    #{
        info => #{
            start => Start,
            stop => Stop,
            duration => Stop - Start
        }
    }).

mozLocation(_MCC, _MNC, _LAC, _CID) ->
    #{
        error => <<"not implemented yet">>
    }.

-ifdef(MozLocation).

% Several links:
% https://github.com/clochix/FxStumbler/blob/fb49753f51451b334a0858ee9a990c31da50ba0f/js/stumbler.js
% https://github.com/kolonist/bscoords/blob/master/lib/bscoords.coffee
%

mozLocationU(MCC, MNC, LAC, CID) ->
    NetType = gsm,
    % ct:pal("mozLocation", []),
    Start = unixtime1000x(),

    % # Mozilla Location API key, random string
    % mozLocationApiKey = (Math.random().toString(36)[2..] for i in [0..2]).join('')[0..31]
    % MozLocationApiKey = random_string(32),
    % MozLocationApiKey = "qojed3w66hqto6r354exveqy1p58kt92",
    MozLocationApiKey = "96989eae-4fd0-4cc5-ad72-770aff066262",

    % https://location.services.mozilla.com/v1/search?key=#{mozLocationApiKey}
    % Url = "https://location.services.mozilla.com:443/v1/search?key=" ++ MozLocationApiKey,
    Url = "https://location.services.mozilla.com/v1/search?key=",
    Header = [{"Content-Type", "application/json"}],
    Request = #{
        cell => #{
            radio => networkType(NetType),
            cid   => CID,
            lac   => LAC,
            mcc   => MCC,
            mnc   => MNC
        }
    },
    Payload = jsx:encode(Request),
    Result = case httpc:request(post, {Url, Header, "application/json; charset=utf-8", Payload}, [{timeout, 5000}, {connect_timeout, 1000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Html}} ->
            case jsxn:decode(Html) of
                #{<<"status">> := <<"ok">>} = Resp ->
                    #{<<"lat">> := Latitude, <<"lon">> := Longitude} = Resp,
                    #{
                        latitude  => Latitude,
                        longitude => Longitude
                    };
                Other ->
                    #{error_format => Other}
            end;
        {ok, {{_, Code, _}, _, Html}} ->
            % ct:pal("mozLocation Code = ~p", [Code]),
            % ct:pal("mozLocation Html = ~p", [Html]),
            Resp = jsxn:decode(Html),
            % ct:pal("mozLocation Resp = ~p", [Resp]),
            #{error => Resp}
    end,
    % Res = httpc:request(post, {Url, Header, "application/json; charset=utf-8", Payload}, [{timeout, 5000}, {connect_timeout, 1000}], [{body_format, binary}]),
    % ct:pal("Res = ~p", [Res]),

    Stop = unixtime1000x(),
    maps:merge(
        Result,
        #{
            info => #{
                start => Start,
                stop => Stop,
                duration => Stop - Start,
                api_key => MozLocationApiKey
                }
        }
    ).

networkType(cdma) -> <<"cdma">>;
networkType(evdo) -> <<"cdma">>;
networkType(ehrpd) -> <<"cdma">>;
networkType(umts) -> <<"umts">>;
networkType(hspa) -> <<"umts">>;
networkType(hsdpa) -> <<"umts">>;
networkType('hspa+') -> <<"umts">>;
networkType(hsupa) -> <<"umts">>;
networkType(lte)  -> <<"lte">>;
networkType(_)    -> <<"gsm">>. % edge, gprs

-endif().

unixtime1000x() ->
    {A, B, C} = os:timestamp(),
    A * 1000000000 + B * 1000 + C div 1000.

% random_string(Length) ->
%     random_string(Length, "0123456789abcdefghijklmnopqrstuvwxyz").
%
% random_string(Length, AllowedChars) ->
%     lists:foldl(fun(_, Acc) ->
%         [lists:nth(random:uniform(length(AllowedChars)),
%         AllowedChars) | Acc]
%     end, [], lists:seq(1, Length)).
