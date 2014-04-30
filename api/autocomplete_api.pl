:- module(autocomplete_api, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_label')).

:- use_module(library(ac_list_util)).
:- use_module(library(instance_search)).

:- rdf_meta
	all_literal_propvalues(r, r, -).

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
	prolog_to_json(Hits, JSON),
	reply_json(json([totalNumberOfResults(TotalNumberOfResults),
			 results(JSON)])).

:- json_object
	hit(uri:uri, property:atom, label:atom, info:_).


/***************************************************
* expand with extra display info
***************************************************/

ac_expand_hit(hit(R,P,L,[]),
	      hit(R,P,L,json([altLabels=Labels,
	      		      images=Images,
			      scopeNotes=ScopeNotes,
			      definitions=Definitions,
			      broader=Broader,
			      narrower=Narrower,
			      related=Related
			     ]))) :-
	all_labels(R,Labels),
	findall(B, rdf_has(R, skos:broader,  B), Broader),
	findall(N, rdf_has(R, skos:narrower, N), Narrower),
	findall(Rl,rdf_has(R, skos:related, Rl), Related),
	findall(Im,rdf_has(R, foaf:depiction, Im), Images),


	all_literal_propvalues(R, skos:scopeNote, ScopeNotes),
	all_literal_propvalues(R, skos:definition, Definitions).


all_literal_propvalues(R,P,Definitions) :-
	findall(json([Lang=Definition]),
		(   rdf_has(R, P, DefLit),
		    literal_text(DefLit,Definition),
		    (	DefLit = literal(lang(Lang, _))
		    ->	true
		    ;	Lang=lang_undefined
		    )
		), Definitions).


all_labels(R,Labels) :-
	findall(AltLabel, (rdf_label(R,Lit),
			   literal_text(Lit, AltLabel)
			  ),
		Labels0),
	sort(Labels0,Labels).
