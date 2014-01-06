%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server - Registry for handler modules
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, add_handler/1, delete_handler/1, 
  get_handlers/0, stop/0]).

%% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {handlers=[]}).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_handler(Handler) ->
  gen_server:cast(?SERVER, {add_handler, Handler}).

delete_handler(Handler) ->
  gen_server:cast(?SERVER, {delete_handler, Handler}).

get_handlers() ->
  gen_server:call(?SERVER, get_handlers).

stop() ->
  gen_server:cast(?SERVER, stop).


%%%============================================================================
%%% behaviour callbacks
%%%============================================================================


init([]) ->
  {ok, #state{}}.

handle_call(get_handlers, _From, State) ->
  {reply, State#state.handlers, State};

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({add_handler, Handler}, State) ->
  {noreply, #state{handlers=[Handler|State#state.handlers]}};

handle_cast({delete_handler, Handler}, State) ->
  NewHandlers = [H || H <- State#state.handlers, H =/= Handler],
  {noreply, #state{handlers=NewHandlers}};

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_UdpMsg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
