erlang-dns
==========

Erlang/OTP DNS server

erlang-dns is an authorative non recursive DNS server I am writing for my site
http://www.domain-name-registration.co.za.

The idea is to have a simple DNS server that can be configured with 
arbitrary Zones via simple Erlang modules. It's up to you whether the
Zone is defined in some file or a DB for example. As long as the module returns
a zone as a list of resource records erlang-dns can serve it up.

In addition, erlang-dns supports custom *extensions* to enhance or entirely change the 
server's behaviour.

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

```erlang
edns:flush("bot.co.za").
```

without restarting the server.

Also, please have a look at the-end-to end test scenario for an example of 
how to setup the included `src/simple_backend.erl` zone provider module.

The `simple_backend` consists essentially of only the following two lines:

```erlang
get_zone(Zone) ->
    {ok, Zone}.
```

Instead of writing the `my_zone_provider` above, you could have achieved the same
result with:

```erlang
Eshell V5.10.2  (abort with ^G)
1> ends:register_zone_provider("bot.co.za", {simple_backend, get_zone, [
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
    ]}).
```

Extensions
==========
Extensions live in the `priv/extensions` folder. Have a look at the `simple_stats` 
extension that counts the number of DNS requests.

Extensions can consist of custom resolvers and OTP servers/supervisors.

Custom Resolvers
----------------
By default, queries are processed by `ed_query_resolver`, however you can 
implement a custom resolver to augment or swap out `ed_query_resolver` entirely.

`edns.app.src` specifies what resolver(s) to use:

```erlang
{env, [
     {port, 1051},
     {resolvers, [ed_query_resolver, simple_stats_resolver]}
   ]}
```

`ed_udp_handler_server` `foldl`s over all resolvers to arrive at a DNS query
response.

The `simple_stats_resolver` is a passthrough resolver that does not modify the repsonse.
All it does is to notify `simple_stats_server` that another query has arrived.

Another example usecase of a custom resolver is one that looks up the synopsis of
a Wikipedia article and returns it in a TXT record.

Custom OTP servers/supervisors
------------------------------
Modules in the `extensions/...` directory that implement either the `gen_server` or
`supervisor` behaviour are automatically started and supervised by `ed_extension_sup`.
This is useful if your custom resolvers need to maintain state.

Tests
=====
To run the end to end test execute `make e2e`.

System Architecture
===================
Please see the wiki https://github.com/hcvst/erlang-dns/wiki/Architecture
![erlang-dns supervision tree](http://stick.im/i/8/8dp.png)

TODO
====
* sanity-check the zone provided by a callback module
* use inet_dns ADT helper functions in the resolver rather than pattern matching.

Notes
=====
Section 4.3.2 Algorithm in http://tools.ietf.org/html/rfc1034
