%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_tcp_server).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0]).

%% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {lsock}).

%%%============================================================================
%%% API
%%%============================================================================


%%-----------------------------------------------------------------------------
%% @doc Start the server
%% @spec start_link(TcpPort::integer()) -> {ok, Pid::pid()}
%% @end
%%-----------------------------------------------------------------------------
start_link(TcpPort) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [TcpPort], []).

%% @doc Start the server on default port
%% @spec start_link() -> {ok, Pid::pid()}
%% @end
start_link() ->
  {ok, Port} = application:get_env(edns, tcp_port),
  start_link(Port).

%%-----------------------------------------------------------------------------
%% @doc Stop the server
%% @spec stop() -> ok
%% @end
%%-----------------------------------------------------------------------------
stop() ->
  gen_server:cast(?SERVER, stop).


%%%============================================================================
%%% behaviour callbacks
%%%============================================================================


init([TcpPort]) ->
  {ok, LSock} = gen_tcp:listen(TcpPort, [{active, true}, binary, {packet, 2}]),
  {ok, #state{lsock=LSock}, 0}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(timeout, #state{lsock=LSock}=State) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  {noreply, State};
handle_info(TcpMsg, #state{lsock=LSock}=State) ->
   io:format("Got tcp: ~p", [TcpMsg]),
   ed_udp_handler_sup:handle(TcpMsg),
   {ok, _Sock} = gen_tcp:accept(LSock),
   {noreply, State}.

terminate(_Reason, #state{lsock = LSock}) ->
  gen_tcp:close(LSock),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
