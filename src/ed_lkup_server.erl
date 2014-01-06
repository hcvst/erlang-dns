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


%%-----------------------------------------------------------------------------
%% @doc Start the server
%% @spec start_link(UdpPort::integer()) -> {ok, Pid::pid()}
%% @end
%%-----------------------------------------------------------------------------
start_link(Packet) ->
  proc_lib:start_link(?MODULE, init, [[self(), Packet]]).

init([Parent, Packet]) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    answer_query(Packet).

answer_query({udp, Socket, IP, Port, ReqBin}) ->
   {ok, Query} = inet_dns:decode(ReqBin),
   Resp = ed_lkup_mngr:lookup(Query), 
   RespBin = inet_dns:encode(Resp),
   gen_udp:send(Socket, IP, Port, RespBin),
   exit(normal).
