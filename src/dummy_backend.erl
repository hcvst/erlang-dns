-module(dummy_backend).

-export([get_zone/1, v1/0]).

-include_lib("kernel/src/inet_dns.hrl").

v1() -> ok.


get_zone(root_zone=Ctx) ->
  %% SOA Data format {MName,RName,Serial,Refresh,Retry,Expiry,Minimum}

  error_logger:info_msg("Got CTX: ~p~n", [Ctx]),
  {ok,[
    #dns_rr{domain="", type=soa, data={
        "SRI-NIC.ARPA",
        "HOSTMASTER.SRI-NIC.ARPA",
        870611,          %serial
        1800,            %refresh every 30 min
        300,             %retry every 5 min
        604800,          %expire after a week
        86400            %minimum of a day
        }
    },
    #dns_rr{domain="", type=ns, data="A.ISI.EDU"},
    #dns_rr{domain="", type=ns, data="C.ISI.EDU"},
    #dns_rr{domain="", type=ns, data="SRI-NIC.ARPA"},

    #dns_rr{domain="MIL", type=ns, data="SRI-NIC.ARPA"},
    #dns_rr{domain="MIL", type=ns, data="A.ISI.EDU"},

    #dns_rr{domain="EDU", type=ns, data="SRI-NIC.ARPA"},
    #dns_rr{domain="EDU", type=ns, data="C.ISI.EDU"},

    #dns_rr{domain="SRI-NIC.ARPA", type=a, data={26,0,0,73}},
    #dns_rr{domain="SRI-NIC.ARPA", type=a, data={10,0,0,51}},
    #dns_rr{domain="SRI-NIC.ARPA", type=mx, data={0, "SRI-NIC.ARPA"}},
    #dns_rr{domain="SRI-NIC.ARPA", type=hinfo, data={"DEC-2060", "TOPS20"}},

    #dns_rr{domain="ACC.ARPA", type=a, data={26,6,0,65}},
    #dns_rr{domain="ACC.ARPA", type=hinfo, data={"PDP-11/70", "UNIX"}},
    #dns_rr{domain="ACC.ARPA", type=mx, data={10, "ACC.ARPA"}},

    #dns_rr{domain="USC-ISIC.ARPA", type=cname, data="C.ISI.EDU"},

    #dns_rr{domain="73.0.0.26.IN-ADDR.ARPA", type=ptr, data="SRI-NIC.ARPA"},
    #dns_rr{domain="65.0.6.26.IN-ADDR.ARPA", type=ptr, data="ACC.ARPA"},
    #dns_rr{domain="51.0.0.10.IN-ADDR.ARPA", type=ptr, data="SRI-NIC.ARPA"},
    #dns_rr{domain="52.0.0.10.IN-ADDR.ARPA", type=ptr, data="C.ISI.EDU"},
    #dns_rr{domain="103.0.3.26.IN-ADDR.ARPA", type=ptr, data="A.ISI.EDU"},

    #dns_rr{domain="A.ISI.EDU", type=a, data={26,3,0,103}},
    #dns_rr{domain="C.ISI.EDU", type=a, data={10,0,0,52}}
  ]};

get_zone(edu_zone) ->
    error;

get_zone(Ctx) ->
  error_logger:info_msg("Got CTX: ~p~n", [Ctx]),
  {ok,[
    #dns_rr{domain="www.abc.com", type=cname, data="www2.abc.com"},
    #dns_rr{domain="www2.abc.com", type=cname, data="abc.com"},
    #dns_rr{domain="*.abc.com", type=a, data={100,2,3,4}},
    #dns_rr{domain="test.abc.com", type=a, data={111,2,3,4}},
    #dns_rr{domain="abc.com", type=a, data={1,2,3,4}},
    #dns_rr{domain="abc.com", type=a, data={5,6,7,8}},
    #dns_rr{domain="sub.abc.com", type=ns, data="ns1.sub.abc.com"},
    #dns_rr{domain="sub.abc.com", type=ns, data="ns2.sub.abc.com"},
    #dns_rr{domain="ns2.sub.abc.com", type=a, data={127,0,0,1}}
  ]}.

  %% Use it: edns:register_zone_provider(ZoneNname, {dummy_backend, get_zone, some_context}).