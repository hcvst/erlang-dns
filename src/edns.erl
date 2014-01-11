%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(edns).

%% API
-export([start/0, stop/0, register_zone_provider/2, flush_zone/1]).


%%%============================================================================
%%% API
%%%============================================================================

start() ->
  application:start(edns).

stop() ->
  application:stop(edns).

register_zone_provider(ZoneName, Handler={_Module, _Function, _Context}) ->
  ed_zone_data_sup:register_zone_provider({ZoneName, Handler}).

flush_zone(ZoneName) ->
  {ok, Zone} = ed_zone_registry_server:get(ZoneName),
  ed_zone_data_server:flush(Zone).

