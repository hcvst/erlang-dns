%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) OTP application
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_app).

-behaviour(application).

%%% behaviour callbacks
-export([start/2, stop/1]).

start(_Type, _Args) ->
  case ed_sup:start_link() of
    {ok, Pid} -> {ok, Pid};
    Other -> {error, Other}
  end.

stop(_State) ->
  ok.
