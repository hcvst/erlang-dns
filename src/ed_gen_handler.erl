-module(ed_gen_handler).

-export([behaviour_info/1, register_handler/1, deregister_handler/1]).

behaviour_info(callbacks) ->
  [
    {register, 0},
    {handle_call, 1}
  ];

behaviour_info(_Other) ->
  undefined.

register_handler(Handler) ->
  ed_registry:add_handler(Handler).

deregister_handler(Handler) ->
  ed_registry:delete_handler(Handler).