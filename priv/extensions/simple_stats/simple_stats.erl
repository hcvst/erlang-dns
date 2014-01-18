-module(simple_stats).

-export([get/0]).

get() ->
    simple_stats_server:get().