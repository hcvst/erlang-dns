%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) supervisor of lookup servers
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_zone_data_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, register_zone_provider/1]).

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

%%-----------------------------------------------------------------------------
%% @doc Starts a child worker to perforom a DNS lookup 
%%-----------------------------------------------------------------------------
register_zone_provider(Args) ->
  supervisor:start_child(?SERVER, [Args]).


%%%============================================================================
%%% behaviour callbacks
%%%============================================================================
init([]) ->
  Server = {ed_zone_data_server, {ed_zone_data_server, start_link, []},
    temporary, 2000, worker, [ed_zone_data_server]},
  Children = [Server],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
