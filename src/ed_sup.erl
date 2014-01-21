%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) root supervisor
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% behaviour callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the supervisor
%% @spec start_link() -> {ok, Pid::pid()}
%%-----------------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%============================================================================
%%% behaviour callbacks
%%%============================================================================
init([]) ->
  UdpServer = {
      ed_udp_server, 
      {ed_udp_server, start_link, []},
      permanent, 2000, worker, 
      [ed_udp_server]
  },
  TcpServer = {
      ed_tcp_server, 
      {ed_tcp_server, start_link, []},
      permanent, 2000, worker, 
      [ed_tcp_server]
  },
  UdpHandlerSup = {
      ed_udp_handler_sup, 
      {ed_udp_handler_sup, start_link, []},
      permanent, 2000, supervisor, 
      [ed_udp_handler_sup]
  },
  ZoneSup = {
      ed_zone_sup, 
      {ed_zone_sup, start_link, []},
      permanent, 2000, supervisor, 
      [ed_zone_sup]
  },
  ExtensionSup = {
      ed_extension_sup, 
      {ed_extension_sup, start_link, []},
      permanent, 2000, supervisor, 
      [ed_extension_sup]
  },
  Children = [UdpServer, TcpServer, UdpHandlerSup, ZoneSup, ExtensionSup],
  RestartStrategy = {one_for_one, 3600, 4},
  {ok, {RestartStrategy, Children}}.
