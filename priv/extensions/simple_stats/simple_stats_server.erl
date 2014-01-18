%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(simple_stats_server).

-behaviour(gen_server).

%% API
-export([start_link/0, get/0, add_query/1]).

%% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get() ->
  gen_server:call(?MODULE, get_stats).

add_query(Q) ->
  gen_server:cast(?MODULE, {add_query, Q}).

%%%============================================================================
%%% behaviour callbacks
%%%============================================================================

init([]) ->
  {ok, 0}.

handle_call(get_stats, _From, State) ->
  {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({add_query, _Q}, State) ->
  {noreply, State+1};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
