%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server - A dummy lookup handler
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_lkup_hndlr).

-behaviour(gen_event).

-include_lib("kernel/src/inet_dns.hrl").

%% API
-export([add_handler/0, delete_handler/0]).

%% behaviour callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
  code_change/3, terminate/2]).


%%%============================================================================
%%% API
%%%============================================================================

add_handler() ->
  ed_lkup_mngr:add_handler(?MODULE, []).

delete_handler() ->
  ed_lkup_mngr:delete_handler(?MODULE, []).


%%%============================================================================
%%% behaviour callbacks
%%%============================================================================

init(_Args) -> 
  {ok, nostate}.

handle_event(_Event, State) ->
  {ok, State}.

handle_call(Req, State) ->
  io:format("Got request: ~p", [Req]),
  {ok, Req, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

