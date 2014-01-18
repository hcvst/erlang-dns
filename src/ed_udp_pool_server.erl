%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_udp_pool_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(NUM_HANDLERS, 5).

-record(state, {sock, handlers=[]}).

%%%============================================================================
%%% API
%%%============================================================================


%%-----------------------------------------------------------------------------
%% @doc Start the server
%% @spec start_link(UdpPort::integer()) -> {ok, Pid::pid()}
%% @end
%%-----------------------------------------------------------------------------
start_link() ->
  error_logger:info_msg("In API start_link"),
  gen_server:start_link(?MODULE, [], []).


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


init([]) ->
  %%error_logger:info_msg("Child starting!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"),
  {ok, #state{}}.

handle_call({take_socket, Socket, Handlers}, _From, State) ->
  %%error_logger:info_msg("Asked to take socket"),
  inet:setopts(Socket, [{active, once}]),
  {reply, ok, #state{sock=Socket, handlers=Handlers}};
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({udp, Socket, IP, Port, ReqBin}, #state{sock=Socket1, handlers=[H1|Hs]}=State) ->
   %%error_logger:info_msg("Got udp ~p !!!!!!!!!!!!!!!!!!!!!!!!!!!", [self()]),
   gen_udp:controlling_process(Socket1, H1),
   gen_server:call(H1, {take_socket, Socket1, Hs++[H1]}),
   gen_udp:send(Socket, IP, Port, ReqBin),
   {noreply, State}.

terminate(_Reason, #state{sock = Sock}) ->
  gen_udp:close(Sock),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
