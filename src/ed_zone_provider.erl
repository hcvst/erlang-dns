%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) - Handler Behaviour and utility functions
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_zone_provider).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [
    {get_zone, 1}
  ].