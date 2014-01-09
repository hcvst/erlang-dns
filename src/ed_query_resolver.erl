-module(ed_query_resolver).

-export([resolve/1]).

-include_lib("kernel/src/inet_dns.hrl").

-define(NO_RECURSION, 0).

resolve(Q) ->
    Q1 = set_recursion_available(Q, ?NO_RECURSION),
    %#dns_rec{
	%   header = _Header, %% dns_header record
	%   qdlist = _QDs,    %% list of question entries
	%   anlist = _ANs,    %% list of answer entries
	%   nslist = _NSs,    %% list of authority entries
	%   arlist = _ARs     %% list of resource entries
    %} = Q1,
    case length(Q1#dns_rec.qdlist) of
    	1 -> find_zone(Q1);
    	_ -> not_implemented_error(Q1, multiple_questions)
    end.

find_zone(Q) -> 
    DomainName = get_domain_name(Q),
    case ed_zone_registry_server:find_nearest_zone(DomainName) of
    	{ok, Zone} -> load_zone(Q, Zone);
    	{error, _} -> non_existent_zone(Q)
    end.

load_zone(Q, Zone) ->
    {ok, RRTree} = ed_zone_data_server:get_zone(Zone),
    match_records(Q, RRTree).

match_records(Q, RRTree) ->
    DomainName = get_domain_name(Q),
    case gb_trees:is_defined(DomainName, RRTree) of
    	true -> 
    	    RRs = gb_trees:get(DomainName, RRTree),
    	    Q#dns_rec{anlist=RRs}; %% @FIXME continue flow here
    	false -> non_existent_domain(Q, RRTree) 
    end.

get_domain_name(Q) ->
    [QD|[]] = Q#dns_rec.qdlist,
    QD#dns_query.domain.

not_implemented_error(Q, Reason) ->
    error_logger:error_msg("Not implemented (~p) Query: ~p", [Reason, Q]),
    set_response_code(Q, ?NOTIMP).

non_existent_zone(Q) ->
    error_logger:info_msg("Zone not found for Query: ~p", [Q]),
    set_response_code(Q, ?NXDOMAIN).

non_existent_domain(Q, _RRTree) ->
    %% @Todo insert SOA
    set_response_code(Q, ?NXDOMAIN).

set_recursion_available(Q, Available) ->
    Header = Q#dns_rec.header#dns_header{ra=Available},
    set_header(Q, Header).

set_response_code(Q, Code) ->
    Header = Q#dns_rec.header#dns_header{rcode=Code},
    set_header(Q, Header).

set_header(Q, Header) ->
    Q#dns_rec{header=Header}.
