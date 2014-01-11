%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%%      This is the example root zone as defined in RFC1034 at 
%%%      http://tools.ietf.org/html/rfc1034#section-6.1
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%---------------------------------------------------------------------------- 

-include_lib("kernel/src/inet_dns.hrl").

-define(SAMPLE_ROOT_ZONE,
 	[                                           %% /!\ TEST SETUP ONLY /!\
    SOA = #dns_rr{domain="", type=soa, data={   %% Assignment is neither 
        "SRI-NIC.ARPA",                         %% required nor recommended
        "HOSTMASTER.SRI-NIC.ARPA",              %% for your own zones.
        870611,          %serial
        1800,            %refresh every 30 min 
        300,             %retry every 5 min
        604800,          %expire after a week
        86400            %minimum of a day
        }
    },
    NS1 = #dns_rr{domain="", type=ns, data="A.ISI.EDU"},
    NS2 = #dns_rr{domain="", type=ns, data="C.ISI.EDU"},
    NS3 = #dns_rr{domain="", type=ns, data="SRI-NIC.ARPA"},

    MilNS1 = #dns_rr{domain="MIL", type=ns, data="SRI-NIC.ARPA"},
    MilNS2 = #dns_rr{domain="MIL", type=ns, data="A.ISI.EDU"},

    EduNS1 = #dns_rr{domain="EDU", type=ns, data="SRI-NIC.ARPA"},
    EduNS2 = #dns_rr{domain="EDU", type=ns, data="C.ISI.EDU"},

    SriA1 = #dns_rr{domain="SRI-NIC.ARPA", type=a, ttl=86400, data={26,0,0,73}},
    SriA2 = #dns_rr{domain="SRI-NIC.ARPA", type=a, ttl=86400, data={10,0,0,51}},
    SriMx = #dns_rr{domain="SRI-NIC.ARPA", type=mx, data={0, "SRI-NIC.ARPA"}},
    SriHi = #dns_rr{domain="SRI-NIC.ARPA", type=hinfo, data={
        "DEC-2060", "TOPS20"}},

    AccA1 = #dns_rr{domain="ACC.ARPA", type=a, data={26,6,0,65}},
    AccHi = #dns_rr{domain="ACC.ARPA", type=hinfo, data={
        "PDP-11/70", "UNIX"}},
    AccMx = #dns_rr{domain="ACC.ARPA", type=mx, data={10, "ACC.ARPA"}},

    UscCname = #dns_rr{domain="USC-ISIC.ARPA", type=cname, data="C.ISI.EDU"},

    Ptr1 = #dns_rr{domain="73.0.0.26.IN-ADDR.ARPA", type=ptr, data=
        "SRI-NIC.ARPA"},
    Ptr2 = #dns_rr{domain="65.0.6.26.IN-ADDR.ARPA", type=ptr, data=
        "ACC.ARPA"},
    Ptr3 = #dns_rr{domain="51.0.0.10.IN-ADDR.ARPA", type=ptr, data=
        "SRI-NIC.ARPA"},
    Ptr4 = #dns_rr{domain="52.0.0.10.IN-ADDR.ARPA", type=ptr, data=
        "C.ISI.EDU"},
    Ptr5 = #dns_rr{domain="103.0.3.26.IN-ADDR.ARPA", type=ptr, data=
        "A.ISI.EDU"},

    AIsiA1 = #dns_rr{domain="A.ISI.EDU", type=a, data={26,3,0,103}},
    CIsiA1 = #dns_rr{domain="C.ISI.EDU", type=a, data={10,0,0,52}}
    ]
).