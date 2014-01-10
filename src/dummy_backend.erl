-module(dummy_backend).

-export([get_zone/1, v1/0]).

-include_lib("kernel/src/inet_dns.hrl").

v1() -> ok.

get_zone(Ctx) ->
  error_logger:info_msg("Got CTX: ~p~n", [Ctx]),
  {ok,[
    #dns_rr{domain="www.abc.com", type=cname, data="www2.abc.com"},
    #dns_rr{domain="www2.abc.com", type=cname, data="abc.com"},
    #dns_rr{domain="abc.com", type=a, data={1,2,3,4}},
    #dns_rr{domain="abc.com", type=a, data={5,6,7,8}}
  ]}.

  %% Use it: edns:register_zone_provider(ZoneNname, {dummy_backend, get_zone, some_context}).