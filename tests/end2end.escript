#!/usr/bin/env escript

%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-include("sample_root_zone.hrl").

-define(DNS_SERVERS, [{{127,0,0,1}, 1053}]).

-define(AUTHORATIVE_ANSWER, true).
-define(NOT_AUTHORATIVE_ANSWER, false).

main(_) ->
    code:add_path("_build/default/lib/edns/ebin"),
    edns:start(),
    edns:register_zone_provider("", {simple_backend, get_zone, 
        ?SAMPLE_ROOT_ZONE}),
    ok = test_rfc1034_6_2_1(),
    ok = test_rfc1034_6_2_2(),
    ok = test_rfc1034_6_2_3(),
    ok = test_rfc1034_6_2_4(),
    ok = test_rfc1034_6_2_5(),
    ok = test_rfc1034_6_2_6(),
    ok = test_rfc1034_6_2_7(),
    ok = test_rfc1034_6_2_8(),
    {ok, 8} = simple_stats:get(),
    log("ALL TESTS PASSED"),
    ok.

test_rfc1034_6_2_1() -> %% http://tools.ietf.org/html/rfc1034#section-6.2.1
    ?SAMPLE_ROOT_ZONE,
    {ok, Msg} = inet_res:nslookup("SRI-NIC.ARPA", in, a, ?DNS_SERVERS),
    ?AUTHORATIVE_ANSWER = inet_dns:header(inet_dns:msg(Msg, header), aa),
    [?SriA1,?SriA2] = inet_dns:msg(Msg, anlist),
    [] = inet_dns:msg(Msg, nslist),
    [] = inet_dns:msg(Msg, arlist),
    log("test_rfc1034_6_2_1 passed"),
    ok.

test_rfc1034_6_2_2() -> %% http://tools.ietf.org/html/rfc1034#section-6.2.2
    ?SAMPLE_ROOT_ZONE,
    {ok, Msg} = inet_res:nslookup("SRI-NIC.ARPA", in, any, ?DNS_SERVERS),
    ?AUTHORATIVE_ANSWER = inet_dns:header(inet_dns:msg(Msg, header), aa),
    [?SriA1, ?SriA2, ?SriMx, ?SriHi] = inet_dns:msg(Msg, anlist),
    [] = inet_dns:msg(Msg, nslist),
    [] = inet_dns:msg(Msg, arlist),
    log("test_rfc1034_6_2_2 passed"),
    ok.

test_rfc1034_6_2_3() -> %% http://tools.ietf.org/html/rfc1034#section-6.2.3
    ?SAMPLE_ROOT_ZONE,
    {ok, Msg} = inet_res:nslookup("SRI-NIC.ARPA", in, mx, ?DNS_SERVERS),
    ?AUTHORATIVE_ANSWER = inet_dns:header(inet_dns:msg(Msg, header), aa),
    [?SriMx] = inet_dns:msg(Msg, anlist),
    [] = inet_dns:msg(Msg, nslist),
    [?SriA1, ?SriA2] = inet_dns:msg(Msg, arlist),
    log("test_rfc1034_6_2_3 passed"),
    ok.

test_rfc1034_6_2_4() -> %% http://tools.ietf.org/html/rfc1034#section-6.2.4
    ?SAMPLE_ROOT_ZONE,
    {ok, Msg} = inet_res:nslookup("SRI-NIC.ARPA", in, ns, ?DNS_SERVERS),
    ?AUTHORATIVE_ANSWER = inet_dns:header(inet_dns:msg(Msg, header), aa),
    [] = inet_dns:msg(Msg, anlist),
    [] = inet_dns:msg(Msg, nslist),
    [] = inet_dns:msg(Msg, arlist),
    log("test_rfc1034_6_2_4 passed"),
    ok.

test_rfc1034_6_2_5() -> %% http://tools.ietf.org/html/rfc1034#section-6.2.5
    ?SAMPLE_ROOT_ZONE,
    {error, nxdomain} = inet_res:nslookup("SIR-NIC.ARPA", in, a, 
        ?DNS_SERVERS),
    log("test_rfc1034_6_2_5 passed but see %FIXME"), % PROBLEMS WITH THIS TEST
    % It doesn't check
    %% ?AUTHORATIVE_ANSWER = inet_dns:header(inet_dns:msg(Msg, header), aa),
    %% [] = inet_dns:msg(Msg, anlist),
    %% [SOA] = inet_dns:msg(Msg, nslist),
    %% [] = inet_dns:msg(Msg, arlist),
    ok.

test_rfc1034_6_2_6() -> %% http://tools.ietf.org/html/rfc1034#section-6.2.6
    ?SAMPLE_ROOT_ZONE,
    {ok, Msg} = inet_res:nslookup("BRL.MIL", in, a, ?DNS_SERVERS),
    ?NOT_AUTHORATIVE_ANSWER = inet_dns:header(inet_dns:msg(Msg, header), aa),
    [] = inet_dns:msg(Msg, anlist),
    [?MilNS1, ?MilNS2] = inet_dns:msg(Msg, nslist),
    [?SriA1, ?SriA2, ?AIsiA1] = inet_dns:msg(Msg, arlist),
    log("test_rfc1034_6_2_6 passed"),
    ok.

test_rfc1034_6_2_7() -> %% http://tools.ietf.org/html/rfc1034#section-6.2.7
    ?SAMPLE_ROOT_ZONE,
    {ok, Msg} = inet_res:nslookup("USC-ISIC.ARPA", in, a, ?DNS_SERVERS),
    ?AUTHORATIVE_ANSWER = inet_dns:header(inet_dns:msg(Msg, header), aa),
    [?UscCname, ?CIsiA1] = inet_dns:msg(Msg, anlist),
    [] = inet_dns:msg(Msg, nslist),
    [] = inet_dns:msg(Msg, arlist),
    log("test_rfc1034_6_2_7 passed"),
    ok.

test_rfc1034_6_2_8() -> %% http://tools.ietf.org/html/rfc1034#section-6.2.8
    ?SAMPLE_ROOT_ZONE,
    {ok, Msg} = inet_res:nslookup("USC-ISIC.ARPA", in, cname, ?DNS_SERVERS),
    ?AUTHORATIVE_ANSWER = inet_dns:header(inet_dns:msg(Msg, header), aa),
    [?UscCname] = inet_dns:msg(Msg, anlist),
    [] = inet_dns:msg(Msg, nslist),
    [] = inet_dns:msg(Msg, arlist),
    log("test_rfc1034_6_2_8 passed"),
    ok.

log(Msg) ->
    io:format("EDNS e2e tests: ~s ~n", [Msg]).
