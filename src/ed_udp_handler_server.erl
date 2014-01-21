%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_udp_handler_server).

%% API
-export([start_link/1]).

%% Callback
-export([init/1]).


%%%============================================================================
%%% API
%%%============================================================================

start_link(Packet) ->
  proc_lib:start_link(?MODULE, init, [[self(), Packet]]).

%%%============================================================================
%%% Callback
%%%============================================================================

init([Parent, Packet]) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  answer_query(Packet),
  exit(normal).

%%%============================================================================
%%% Internal
%%%============================================================================

answer_query({udp, Socket, IP, Port, ReqBin}) ->
  io:format("UDP ReqBin: ~p", [ReqBin]),
  {ok, Query} = inet_dns:decode(ReqBin),
  {ok, Resolvers} = application:get_env(edns, resolvers),
  Resp = lists:foldl(
  	fun(R, Acc) ->
  	  R:resolve(Acc) 
  	end, Query, Resolvers),
  RespBin = inet_dns:encode(Resp),
  gen_udp:send(Socket, IP, Port, RespBin);
answer_query({tcp, Socket, ReqBin}) ->
  io:format("TCP ReqBin: ~p", [ReqBin]),
  {ok, Query} = inet_dns:decode(ReqBin),
  {ok, Resolvers} = application:get_env(edns, resolvers),
  Resp = lists:foldl(
    fun(R, Acc) ->
      R:resolve(Acc) 
    end, Query, Resolvers),
  RespBin = inet_dns:encode(Resp),
  gen_tcp:send(Socket, RespBin);
answer_query(Other) ->
  io:format("Other: ~p", [Other]), 
  ok.