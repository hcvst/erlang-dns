%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_udp_server).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0]).

%% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock}).

%%%============================================================================
%%% API
%%%============================================================================


%%-----------------------------------------------------------------------------
%% @doc Start the server
%% @spec start_link(UdpPort::integer()) -> {ok, Pid::pid()}
%% @end
%%-----------------------------------------------------------------------------
start_link(UdpPort) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [UdpPort], []).

%% @doc Start the server on default port
%% @spec start_link() -> {ok, Pid::pid()}
%% @end
start_link() ->
  {ok, Port} = application:get_env(edns, udp_port),
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


init([UdpPort]) ->
  {ok, Sock} = gen_udp:open(UdpPort, [{active, true}, binary]),
  {ok, #state{sock=Sock}}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(UdpMsg, State) ->
   ed_udp_handler_sup:handle(UdpMsg),
   {noreply, State}.

terminate(_Reason, #state{sock = Sock}) ->
  gen_udp:close(Sock),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
