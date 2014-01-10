%%% 4.3.2. Algorithm
%%% 
%%% The actual algorithm used by the name server will depend on the local OS
%%% and data structures used to store RRs.  The following algorithm assumes
%%% that the RRs are organized in several tree structures, one for each
%%% zone, and another for the cache:
%%% 
%%%    1. Set or clear the value of recursion available in the response
%%%       depending on whether the name server is willing to provide
%%%       recursive service.  If recursive service is available and
%%%       requested via the RD bit in the query, go to step 5,
%%%       otherwise step 2.
%%% 
%%%    2. Search the available zones for the zone which is the nearest
%%%       ancestor to QNAME.  If such a zone is found, go to step 3,
%%%       otherwise step 4.
%%% 
%%%    3. Start matching down, label by label, in the zone.  The
%%%       matching process can terminate several ways:
%%% 
%%%          a. If the whole of QNAME is matched, we have found the
%%%             node.
%%% 
%%%             If the data at the node is a CNAME, and QTYPE doesn't
%%%             match CNAME, copy the CNAME RR into the answer section
%%%             of the response, change QNAME to the canonical name in
%%%             the CNAME RR, and go back to step 1.
%%% 
%%%             Otherwise, copy all RRs which match QTYPE into the
%%%             answer section and go to step 6.
%%% 
%%%          b. If a match would take us out of the authoritative data,
%%%             we have a referral.  This happens when we encounter a
%%%             node with NS RRs marking cuts along the bottom of a
%%%             zone.
%%% 
%%%             Copy the NS RRs for the subzone into the authority
%%%             section of the reply.  Put whatever addresses are
%%%             available into the additional section, using glue RRs
%%%             if the addresses are not available from authoritative
%%%             data or the cache.  Go to step 4.
%%% 
%%%          c. If at some label, a match is impossible (i.e., the
%%%             corresponding label does not exist), look to see if a
%%%             the "*" label exists.
%%% 
%%%             If the "*" label does not exist, check whether the name
%%%             we are looking for is the original QNAME in the query
%%% 
%%% Mockapetris                                                    [Page 24]
%%% 
%%% RFC 1034             Domain Concepts and Facilities        November 1987
%%% 
%%%             or a name we have followed due to a CNAME.  If the name
%%%             is original, set an authoritative name error in the
%%%             response and exit.  Otherwise just exit.
%%% 
%%%             If the "*" label does exist, match RRs at that node
%%%             against QTYPE.  If any match, copy them into the answer
%%%             section, but set the owner of the RR to be QNAME, and
%%%             not the node with the "*" label.  Go to step 6.
%%% 
%%%    4. Start matching down in the cache.  If QNAME is found in the
%%%       cache, copy all RRs attached to it that match QTYPE into the
%%%       answer section.  If there was no delegation from
%%%       authoritative data, look for the best one from the cache, and
%%%       put it in the authority section.  Go to step 6.
%%% 
%%%    5. Using the local resolver or a copy of its algorithm (see
%%%       resolver section of this memo) to answer the query.  Store
%%%       the results, including any intermediate CNAMEs, in the answer
%%%       section of the response.
%%% 
%%%    6. Using local data only, attempt to add other RRs which may be
%%%       useful to the additional section of the query.  Exit.

% also see http://bind10.isc.org/wiki/AuthServerQueryLogic

-module(ed_query_resolver).

%% API
-export([resolve/1]).

-include_lib("kernel/src/inet_dns.hrl").

-define(AUTHORATIVE_ANSWER, 1).
-define(NO_RECURSION, 0).


%%%============================================================================
%%% Server Algorithm - see RFC1034 (http://tools.ietf.org/html/rfc1034)
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
    case is_referral_match(RRTree, Name) of
    	true ->  process_referral_match(Q, RRTree, Name);
    	false ->
    	    case is_qname_match(RRTree, Name, D) of
    	    	true -> process_qname_match(Q, RRTree, Name, T);
    	    	false ->
    	    	    case is_wildcard_match() of
    	    	        true -> process_wildcard_match(Q, RRTree, Name, T);
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

is_referral_match(RRTree, DomainName) ->
    % ... If a match would take us out of the authoritative data,
    % we have a referral.  This happens when we encounter a
    % node with NS RRs marking cuts along the bottom of a
    % zone.
    %
    % Copy the NS RRs for the subzone into the authority
    % section of the reply.  Put whatever addresses are
    % available into the additional section, using glue RRs
    % if the addresses are not available from authoritative
    % data or the cache.  Go to step 4.
    case gb_trees:lookup(DomainName, RRTree) of
    	none -> false;
    	{value, RRs} ->
    	    lists:all(fun(RR) -> RR#dns_rr.type =:= ns end, RRs)
    end.

process_referral_match(Q, RRTree, DomainName) ->
    NsList = Q#dns_rec.nslist,	
    ArList = Q#dns_rec.arlist,
    NsRRs = gb_trees:get(DomainName, RRTree),
    GlueRRs = lists:foldr(
    	fun(NsRR, Acc) ->
    	    case gb_trees:lookup(NsRR#dns_rr.data, RRTree) of
    	        none -> Acc;
    	        {value, RRs} -> lists:foldr(
    	        	fun(RR, Acc1) ->
    	        		case RR#dns_rr.type =:= a of
    	        			false -> Acc1;
    	        			true -> [RR|Acc1]
    	        		end
    	        	end
    	        	, Acc, RRs)
    	    end 
    	end 
    	, [], NsRRs),
    Q#dns_rec{nslist=NsList++NsRRs, arlist=ArList++GlueRRs}.

is_wildcard_match() ->
    false. % TODO

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

get_soa_rr(RRTree) ->
    RRs = lists:flatten(gb_trees:values(RRTree)),
    [SOA|[]] = lists:filter(
    	fun(RR) -> RR#dns_rr.type =:= soa end
    	, RRs),
    SOA.


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
