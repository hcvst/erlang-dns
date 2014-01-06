%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_lkup_mngr).

%% API
-export([
  start_link/0, 
  add_handler/2,
  delete_handler/2,
  lookup/1
]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================


%%-----------------------------------------------------------------------------
%% @doc Start Lookup Manager
%% @spec start_link() -> {ok, Pid::pid()}
%% @end
%%-----------------------------------------------------------------------------
start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  error_logger:info_msg("Added new handler: ~p", [Handler]),
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?SERVER, Handler, Args).

lookup(Query) ->
  Handlers = gen_event:which_handlers(?SERVER),
  lists:foldl(fun(H,Acc) -> gen_event:call(?SERVER, H, Acc) end, 
    Query, Handlers).
