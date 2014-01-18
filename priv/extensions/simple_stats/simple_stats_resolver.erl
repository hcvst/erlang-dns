-module(simple_stats_resolver).

-export([resolve/1]).

resolve(Q) ->
    simple_stats_server:add_query(Q),
    Q.