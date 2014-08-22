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
			 /* FIX ME
			 offset(Offset,
				[default(0)]),
			 */
			 tokenized(Tokenized,
				   [ float,
				     default(0.5),
				     description('Ratio of tokenized versus pure prefix search results')
				   ]),
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
	instance_search(Query, HitsPrefix,    [tokenize(false) | Options]),
	instance_search(Query, HitsTokenized, [tokenize(true) | Options]),
	length(HitsPrefix, NrHitsPrefix),
	length(HitsTokenized, NrHitsTokenized),
	TotalNumberOfResults is NrHitsPrefix + NrHitsTokenized,

	FirstHalf is min(floor(Limit/(1 -Tokenized)), NrHitsPrefix),
	SecondHalf is Limit - FirstHalf,

	% list_offset(Hits0, Offset, Hits1),

	list_limit(HitsPrefix, FirstHalf, PrefixHits, _),
	list_limit(HitsTokenized, SecondHalf,TokenizedHits, _),
	append([PrefixHits, TokenizedHits], Hits2),
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
	findall(Rel, skos_related_concept(R, Rel), Related),
	findall(Bro, skos_parent_child(Bro, R), Broader),
	findall(Nar, skos_parent_child(R, Nar), Narrower),
	findall(Img, rdf_has(R, foaf:depiction, Img), Images),

	json_all_literal_propvalues(R, skos:notation, Notations),
	json_all_literal_propvalues(R, skos:scopeNote, ScopeNotes),
	json_all_literal_propvalues(R, skos:definition, Definitions).


