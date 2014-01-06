%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(edns).

%% API
-export([start/0, stop/0]).


%%%============================================================================
%%% API
%%%============================================================================

start() ->
  application:start(edns),
  ed_log_handler:register({}).

stop() ->
  application:stop(edns).
