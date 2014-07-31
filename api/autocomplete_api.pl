:- module(autocomplete_api, []).

:- use_module(library(apply)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(library(skos/util)).
:- use_module(library(skos/json)).

:- use_module(library('semweb/rdf_db')).

:- use_module(library(ac_list_util)).
:- use_module(library(instance_search)).

:- http_handler(api(autocomplete), http_autocomplete, [js(true)]).

http_autocomplete(Request) :-
	http_parameters(Request,
			[q(Query,
			   []),
			 limit(Limit,
			       [default(10)]),
			 offset(Offset,
				[default(0)]),
			 method(Method,
				[one_of([prefix,stem,exact]),
				 default(prefix),
				 description('String matching method')
				]),
			 labelrank(LabelRankingAtom,
				   [atom,
				    default('[]'),
				    description('Property/value pairs ranking the labels, see instance_search/3')
				   ]),
			 filter(Filter,
				[json_filter,
				 default([]),
				 description('JSON object specifying a restriction on the results')])
			]),
	term_to_atom(LabelRankingList, LabelRankingAtom),
	Options = [match(Method),
		   filter(Filter),
		   property(LabelRankingList)],
	instance_search(Query, Hits0, Options),
	length(Hits0, TotalNumberOfResults),
	list_offset(Hits0, Offset, Hits1),
	list_limit(Hits1, Limit, Hits2, _),
        maplist(ac_expand_hit, Hits2,Hits),
	reply_json(json{totalNumberOfResults:TotalNumberOfResults,
			results:Hits}).


/***************************************************
* expand with extra display info
***************************************************/

ac_expand_hit(hit(R,P,_Label,[]),
	      json{uri:R,
		   property:P,
		   label:MainLabel,
		   info: info{altLabels:Labels,
			      images:Images,
			      scopeNotes:ScopeNotes,
			      definitions:Definitions,
			      notations:Notations,
			      broader:Broader,
			      narrower:Narrower,
			      related:Related
			     }
		  }) :-
	skos_all_labels(R,Labels),
	skos_notation_ish(R,MainLabel),
	skos_related_concepts(R, Related),
	findall(N, rdf_has(R, skos:notation,  N), Notations),
	findall(B, rdf_has(R, skos:broader,  B), Broader),
	findall(N, rdf_has(R, skos:narrower, N), Narrower),
	findall(Im,rdf_has(R, foaf:depiction, Im), Images),

	json_all_literal_propvalues(R, skos:scopeNote, ScopeNotes),
	json_all_literal_propvalues(R, skos:definition, Definitions).


