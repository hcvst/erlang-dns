%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_lkup_server).

%% API
-export([start_link/1, init/1]).


%%%============================================================================
%%% API
%%%============================================================================

start_link(Packet) ->
  proc_lib:start_link(?MODULE, init, [[self(), Packet]]).

init([Parent, Packet]) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  answer_query(Packet).

%%%============================================================================
%%% Internal
%%%============================================================================

answer_query({udp, Socket, IP, Port, ReqBin}) ->
  {ok, Query} = inet_dns:decode(ReqBin),
  Resp = call_handlers(Query), 
  RespBin = inet_dns:encode(Resp),
  gen_udp:send(Socket, IP, Port, RespBin),
  exit(normal).

call_handlers(Query) ->
  Handlers = ed_registry:get_handlers(),
  lists:foldl(fun(Handler, Acc) -> Handler:handle_call(Acc) end,
  	Query, Handlers).