%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_zone_registry_server).

-behaviour(gen_server).

%% API
-export([start_link/0, register/2, deregister/1, get/1, find_nearest_zone/1]).

%% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(ZoneName, Pid) ->
  gen_server:call(?SERVER, {register, ZoneName, Pid}).

deregister(ZoneName) ->
  gen_server:call(?SERVER, {deregister, ZoneName}).

get(ZoneName) ->
  gen_server:call(?SERVER, {get, ZoneName}).

find_nearest_zone(DomainName) ->
  gen_server:call(?SERVER, {find_nearest_zone, DomainName}).


%%%============================================================================
%%% behaviour callbacks
%%%============================================================================


init([]) ->
  {ok, gb_trees:empty()}.

handle_call({register, ZoneName, Pid}, _From, State) ->
  case gb_trees:is_defined(ZoneName, State) of
  	false ->
  	  error_logger:info_msg("Zone ~p registered.", [ZoneName]), 
  	  {reply, ok, gb_trees:insert(ZoneName, Pid, State)};
  	true -> 
  	  {reply, {error, zone_already_registered}, State}
  end;
handle_call({deregister, ZoneName}, _From, State) ->
  case gb_trees:is_defined(ZoneName, State) of
  	true -> 
  	  error_logger:info_msg("Zone ~p deregistered.", [ZoneName]),
  	  {reply, ok, gb_trees:delete(ZoneName, State)};
  	false -> 
  	  {reply, {error, zone_not_registered}, State}
  end;
handle_call({get, ZoneName}, _From, State) -> 
  case gb_trees:is_defined(ZoneName, State) of
  	true -> 
  	  {reply, {ok, gb_trees:get(ZoneName,State)}, State};
  	false -> 
  	  {reply, {error, zone_not_registered}, State}
  end;
handle_call({find_nearest_zone, DomainName}, _From, State) ->
  NameTails = ed_utils:tails(string:tokens(DomainName, ".")),
  Names = lists:map(fun(X) -> string:join(X, ".") end, NameTails),
  IsZoneNotDefined = fun(Z) -> not gb_trees:is_defined(Z, State) end,
  Zone = case lists:dropwhile(IsZoneNotDefined, Names) of
  	[] -> {error, zone_not_found};
  	[H|_] -> {ok, gb_trees:get(H, State)}
  end,
  {reply, Zone, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
