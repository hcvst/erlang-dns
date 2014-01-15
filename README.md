erlang-dns
==========

Erlang/OTP DNS server

EDNS is an authorative non recursive DNS server I am writing for my site
http://www.domain-name-registration.co.za.

The idea is to have a simple DNS server that can be configured with 
arbitrary Zones via simple Erlang modules. It's up to you whether the
Zone is defined in some file or a DB for example. As long as the module returns
a zone as a list of resource records EDNS can serve it up.

Releases
========
* 2014-01-11 `v0.1-alpha` - First version that conforms to RFC1034 examples

Example
=======

```erlang
-module(my_zone_provider).

-export([get_zone/1]).

-include_lib("kernel/src/inet_dns.hrl").

get_zone(_Args) ->
    %% Fetch the Zone from a file, the DB, ...
    %% Here we just hardcode it
    {ok, [                                     
        #dns_rr{domain="bot.co.za", type=soa, data={   
            "ns1.bot.co.za",                         
            "hc.vst.io",              
            870611,          %serial
            1800,            %refresh every 30 min 
            300,             %retry every 5 min
            604800,          %expire after a week
            86400            %minimum of a day
            }
        },
        #dns_rr{domain="bot.co.za", type=ns, data="ns1.bot.co.za"},
        #dns_rr{domain="bot.co.za", type=ns, data="ns2.bot.co.za"},
        #dns_rr{domain="www.bot.co.za", type=cname, data="bot.co.za"},
        #dns_rr{domain="bot.co.za", type=a, data={127,0,0,1}}
    ]}.
```

Then, bring up the server with `erl -pa ./ebin -s edns` and register your zone 
provider.

```erlang
edns:register_zone_provider("bot.co.za", {my_zone_provider, get_zone, []}).
```

If you change and recompile `my_zone_provider` you can flush the zone with

```
edns:flush("bot.co.za").
```

without restarting the server.

Also, please have a look at the-end-to end test scenario for an example of 
how to setup the included `src/simple_backend.erl` zone provider module.

Tests
=====
To run the end to end test execute `make e2e`.

System Architecture
===================
Please see the wiki https://github.com/hcvst/erlang-dns/wiki/Architecture
![erlang-dns supervision tree](http://stick.im/i/l/lVU.png)

TODO
====
* sanity-check the zone provided by a callback module
* use inet_dns ADT helper functions in the resolver rather than pattern matching.

Notes
=====
* Section 4.3.2 Algorithm in http://tools.ietf.org/html/rfc1034
* https://erlangcentral.org/wiki/index.php/Building_a_Non-blocking_TCP_server_using_OTP_principles
