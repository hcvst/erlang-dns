%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) - Handler Behaviour and utility functions
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_gen_handler).

-export([behaviour_info/1, register_handler/2, call_handlers/1]).

behaviour_info(callbacks) ->
  [
    {register, 1},
    {handle_call, 2}
  ];

behaviour_info(_Other) ->
  undefined.

register_handler(Handler, Context) ->
  ed_registry:add_handler({Handler, Context}).

call_handlers(Query) ->
  lists:foldl(
  	fun({Handler, Context}, Acc) -> 
  		Handler:handle_call(Acc, Context) 
  	end, Query, ed_registry:get_handlers()).