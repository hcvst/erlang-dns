%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) root supervisor
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_extension_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% behaviour callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(EXTENSIONS_PATH, "priv/extensions/*/*.erl").

-define(CHILD(M), {M, {M, start_link, []}, permanent, 5000, worker, [M]}).

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
  Children = find_extensions(?EXTENSIONS_PATH),
  RestartStrategy = {one_for_one, 3600, 4},
  {ok, {RestartStrategy, Children}}.

find_extensions(Path) ->
  FilePaths = filelib:wildcard(Path),
  FileNames = lists:map(fun filename:basename/1, FilePaths),
  Modules = lists:map(
  	fun(F) ->
  		F1 = re:replace(F, ".erl", "", [{return, list}]),
  		list_to_atom(F1)
  	end, FileNames),
  OtpModules = lists:filter(
  	fun(M) ->
  		Attrs = M:module_info(attributes),
  		case proplists:get_value(behaviour, Attrs) of
  			[gen_server] -> true;
  			[supervisor] -> true;
  			_ -> false
  		end
  	end, Modules),
  case OtpModules of
  	[] -> nop;
  	_ -> error_logger:info_msg("Found extensions: ~p", [OtpModules])
  end,
  [?CHILD(M) || M <- OtpModules].
