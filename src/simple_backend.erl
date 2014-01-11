%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%%      The simple backend demonstrates how to provide a callback module to
%%%      the DNS server.
%%%  
%%%      Callback modules are registered with the server using
%%%          ends:register_zone_provider/2.
%%%      and are expected to return a list of #dns_rr records.
%%%
%%%      For an example of how to use this backend, please see end2end.escript
%%%      in the tests directory, which also contains a sample zone definition
%%       in sample_root_zone.hrl.
%%%      
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------


-module(simple_backend).

-export([get_zone/1]).

get_zone(Zone) ->
    {ok, Zone}.