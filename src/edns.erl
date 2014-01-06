%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(edns).

%% API
-export([start/0, stop/0]).

start() ->
  application:start(edns),
  ed_lkup_hndlr:add_handler().

stop() ->
  application:stop(edns).
