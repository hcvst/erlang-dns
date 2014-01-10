-module(ed_query_resolver).
%% also see http://bind10.isc.org/wiki/AuthServerQueryLogic
%% API
-export([resolve/1]).

-include_lib("kernel/src/inet_dns.hrl").

-define(AUTHORATIVE_ANSWER, 1).
-define(NO_RECURSION, 0).



%%%============================================================================
%%% Server Algorithm - see RFC1034
%%%============================================================================

resolve(Q) ->
    Q1 = set_recursion_available(Q, ?NO_RECURSION),
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
    Q1 = set_aa_header_flag(Q),
    {ok, RRTree} = ed_zone_data_server:get_zone(Zone),
    match_records(Q1, RRTree).

match_records(Q, RRTree) ->
    DomainName = get_domain_name(Q),  
    NameTails = ed_utils:tails(string:tokens(DomainName, ".")),
    Names = lists:map(fun(X) -> string:join(X, ".") end, NameTails),
    match_records(Q, RRTree, Names).

match_records(Q, RRTree, []) ->
    non_existent_domain(Q, RRTree);
match_records(Q, RRTree, [Name|Rest]) ->
    [#dns_query{domain=D, type=T, class=_C}|[]] = Q#dns_rec.qdlist,
    case is_qname_match(RRTree, Name, D) of
    	true ->  process_qname_match(Q, RRTree, D, T);
    	false ->
    	    case is_referral_match() of
    	    	true -> process_referral_match(Q, RRTree, D, T);
    	    	false ->
    	    	    case is_wildcard_match() of
    	    	        true -> process_wildcard_match(Q, RRTree, D, T);
    	    	        false ->
    	    	            match_records(Q, RRTree, Rest)
    	    	    end
    	    end
    end.

is_qname_match(RRTree, DomainName, DomainName) ->
    gb_trees:is_defined(DomainName, RRTree);
is_qname_match(_RRTree, _DomainName, _OtherDomainName) ->
    false.

process_qname_match(Q, RRTree, DomainName, Type) ->
    case gb_trees:get(DomainName, RRTree) of
    	[RR|[]] when RR#dns_rr.type=:=cname andalso Type=/=cname ->
    	    resolve_cname(Q, RR);
    	RRs ->
    	    matching_rr_to_anlist(Q, RRs, Type)
    end.

resolve_cname(Q, RR) ->
    % ...copy the CNAME RR into the answer section
    % of the response, change QNAME to the canonical name in
    % the CNAME RR, and go back to step 1.
    [QD|[]] = Q#dns_rec.qdlist,
    AnList = Q#dns_rec.anlist,
    CName = RR#dns_rr.data,
    Q1 = Q#dns_rec{
        qdlist=[QD#dns_query{domain=CName}],
        anlist=[RR|AnList]
    },
    Q2 = resolve(Q1),
    Q2#dns_rec{qdlist=Q#dns_rec.qdlist}.

matching_rr_to_anlist(Q, RRs, Type) ->
    MatchingRRs = lists:filter(
    	fun(RR) -> RR#dns_rr.type =:= Type end
    	, RRs),
    AnList = Q#dns_rec.anlist,	
    Q#dns_rec{anlist=AnList++MatchingRRs}.

is_referral_match() ->
    true. % TODO

process_referral_match(Q, _RRTree, _D, _T) ->
    Q. % TODO

is_wildcard_match() ->
    true. % TODO

process_wildcard_match(Q, _RRTree, _D, _T) ->
    Q. % TODO  

not_implemented_error(Q, Reason) ->
    error_logger:error_msg("Not implemented (~p) Query: ~p", [Reason, Q]),
    set_response_code(Q, ?NOTIMP).

non_existent_zone(Q) ->
    error_logger:info_msg("Zone not found for Query: ~p", [Q]),
    case is_cname_recursive_lookup(Q) of
    	true  -> set_response_code(Q, ?NOERROR);
    	false -> set_response_code(Q, ?REFUSED)
    end.

non_existent_domain(Q, _RRTree) ->
    %% @Todo insert SOA
    set_response_code(Q, ?NXDOMAIN).

%%%============================================================================
%%% Helpers
%%%============================================================================

is_cname_recursive_lookup(Q) ->
    % If we invoked resolve/1 recursively to resolve a CNAME reference then
    % the answer section should already contain exactly one CNAME RR and
    % none otherwise. See rfc1034 section 4.3.2.3.a.
    case Q#dns_rec.anlist of
    	[RR|[]] when RR#dns_rr.type=:=cname -> true;
    	[] -> false
    end.

get_domain_name(Q) ->
    [QD|[]] = Q#dns_rec.qdlist,
    QD#dns_query.domain.

set_recursion_available(Q, Available) ->
    Header = Q#dns_rec.header#dns_header{ra=Available},
    set_header(Q, Header).

set_aa_header_flag(Q) ->
    Header = Q#dns_rec.header#dns_header{aa=?AUTHORATIVE_ANSWER},
    set_header(Q, Header).

set_response_code(Q, Code) ->
    Header = Q#dns_rec.header#dns_header{rcode=Code},
    set_header(Q, Header).

set_header(Q, Header) ->
    Q#dns_rec{header=Header}.
