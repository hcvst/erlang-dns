%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(edns).

%% API
-export([start/0, stop/0, register_zone_provider/1]).


%%%============================================================================
%%% API
%%%============================================================================

start() ->
  application:start(edns).

stop() ->
  application:stop(edns).

register_zone_provider(_Args) ->
  notimplemented.
