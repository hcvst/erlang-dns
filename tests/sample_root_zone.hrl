%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%%      This is the example root zone as defined in RFC1034 at 
%%%      http://tools.ietf.org/html/rfc1034#section-6.1
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%---------------------------------------------------------------------------- 

-include_lib("kernel/src/inet_dns.hrl").

-define(SOA, SOA). %% All these DEFINEs here are just to make the end2end test
-define(NS1, NS1). %% case a little bit more robust. NO NEED in your modules.
-define(NS2, NS2). 
-define(NS3, NS3). 

-define(MilNS1, MilNS1). 
-define(MilNS2, MilNS2). 

-define(EduNS1,EduNS1).
-define(EduNS2, EduNS2). 

-define(SriA1, SriA1). 
-define(SriA2, SriA2). 
-define(SriMx, SriMx). 
-define(SriHi, SriHi). 

-define(AccA1, AccA1). 
-define(AccHi, AccHi).
-define(AccMx, AccMx). 

-define(UscCname, UscCname). 

-define(Ptr1, Ptr1). 
-define(Ptr2, Ptr2).
-define(Ptr3, Ptr3).
-define(Ptr4, Ptr4).
-define(Ptr5, Ptr5).

-define(AIsiA1, AIsiA1). 
-define(CIsiA1, CIsiA1). 

-define(SAMPLE_ROOT_ZONE,
 	[                                           %% /!\ TEST SETUP ONLY /!\
    ?SOA = #dns_rr{domain="", type=soa, data={   %% Assignment is neither 
        "SRI-NIC.ARPA",                         %% required nor recommended
        "HOSTMASTER.SRI-NIC.ARPA",              %% for your own zones.
        870611,          %serial
        1800,            %refresh every 30 min 
        300,             %retry every 5 min
        604800,          %expire after a week
        86400            %minimum of a day
        }
    },
    ?NS1 = #dns_rr{domain="", type=ns, data="a.isi.edu"},
    ?NS2 = #dns_rr{domain="", type=ns, data="c.isi.edu"},
    ?NS3 = #dns_rr{domain="", type=ns, data="sri-nic.arpa"},

    ?MilNS1 = #dns_rr{domain="mil", type=ns, data="sri-nic.arpa"},
    ?MilNS2 = #dns_rr{domain="mil", type=ns, data="a.isi.edu"},

    ?EduNS1 = #dns_rr{domain="edu", type=ns, data="sri-nic.arpa"},
    ?EduNS2 = #dns_rr{domain="edu", type=ns, data="c.isi.edu"},

    ?SriA1 = #dns_rr{domain="sri-nic.arpa", type=a, ttl=86400, data={26,0,0,73}},
    ?SriA2 = #dns_rr{domain="sri-nic.arpa", type=a, ttl=86400, data={10,0,0,51}},
    ?SriMx = #dns_rr{domain="sri-nic.arpa", type=mx, data={0, "sri-nic.arpa"}},
    ?SriHi = #dns_rr{domain="sri-nic.arpa", type=hinfo, data={
        "DEC-2060", "TOPS20"}},

    ?AccA1 = #dns_rr{domain="acc.arpa", type=a, data={26,6,0,65}},
    ?AccHi = #dns_rr{domain="acc.arpa", type=hinfo, data={
        "PDP-11/70", "UNIX"}},
    ?AccMx = #dns_rr{domain="acc.arpa", type=mx, data={10, "acc.arpa"}},

    ?UscCname = #dns_rr{domain="usc-isic.arpa", type=cname, data="c.isi.edu"},

    ?Ptr1 = #dns_rr{domain="73.0.0.26.in-addr.arpa", type=ptr, data=
        "sri-nic.arpa"},
    ?Ptr2 = #dns_rr{domain="65.0.6.26.in-addr.arpa", type=ptr, data=
        "acc.arpa"},
    ?Ptr3 = #dns_rr{domain="51.0.0.10.in-addr.arpa", type=ptr, data=
        "sri-nic.arpa"},
    ?Ptr4 = #dns_rr{domain="52.0.0.10.in-addr.arpa", type=ptr, data=
        "c.isi.edu"},
    ?Ptr5 = #dns_rr{domain="103.0.3.26.in-addr.arpa", type=ptr, data=
        "a.isi.edu"},

    ?AIsiA1 = #dns_rr{domain="a.isi.edu", type=a, data={26,3,0,103}},
    ?CIsiA1 = #dns_rr{domain="c.isi.edu", type=a, data={10,0,0,52}}
    ]
).