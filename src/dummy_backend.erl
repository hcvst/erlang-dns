-module(dummy_backend).

-export([get_zone/1]).

-include_lib("kernel/src/inet_dns.hrl").

get_zone(Ctx) ->
  error_logger:info_msg("Got CTX: ~p~n", [Ctx]),
  {ok,[
    #dns_rr{domain="abc.com", type="A"},
    #dns_rr{domain="def.com", type="A"},
    #dns_rr{domain="abc.com", type="MX"},
    #dns_rr{domain="www.abc.com", type="A"}
  ]}.

  %% Use it: edns:register_zone_provider(ZoneNname, {dummy_backend, get_zone, some_context}).