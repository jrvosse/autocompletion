:- module(ac_instance_search,
	[ instance_search/3		% +Query, -Hits, +Options

	]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(find_resource)).
:- use_module(library(ac_filter)).

/***************************************************
* term search
***************************************************/

%%	instance_search(+Query, -Hits:hit(uri,property,label,info), +Options)
%
%	Hits contains all resources matching Query.
%	Options are
%		match = prefix;stem;exact
%		filter =
%		compound = boolean
%		property = [property-score]
%		shortQueriesOptimized = integer
%		treeRemove = false;relation


instance_search(Query, Hits, Options) :-
	option(property(Property), Options, []),
	label_list(Property, LabelList),
	find_resources(Query, LabelList, Hits, Options).


/***************************************************
* literal matching
***************************************************/

%%	label_list(+Property, -LabelList)
%
%	LabelList is a list of Pairs with an rdf property
%	score. Lower score gets preference.

label_list([], LabelList) :- !,
	rdf_equal(rdfs:label, Label),
	rdf_equal(skos:prefLabel, PrefLabel),
	rdf_equal(skos:notation, Notation),
	LabelList = [Notation - 0,
		     PrefLabel - 1,
		     Label-2
	].
label_list(Property, LabelList) :-
	atom(Property), !,
	LabelList = [Property-0].
label_list(List, LabelList) :-
    is_list(List),
    format_label_list(List, 0, LabelList),
    !.
label_list(List,_) :-
	 type_error(labellist,  List).

format_label_list([], _, []).
format_label_list([H|T], N0, [P-N|Rest]) :-
	N1 is N0+0.1,
	(   atom(H)
	->  N = N0,
	    P = H
	;   H = P-N,
	    number(N)
	->  true
	;   H = pair(P,N)
	->  true
	),
	format_label_list(T, N1, Rest).


%%	find_resources(+Query, +LabelList, -Hits, Options)
%
%	Hits contains uris with prefix matching label.

find_resources(Query, LabelList, Hits, Options0) :-
	Options = [distance(false),attributes(LabelList)|Options0],
	find_resource_by_name(Query, Hits0, Options),
	maplist(ac_hit, Hits0, Hits1),
	filter(Hits1, Hits, Options0).

ac_hit(hit(_D,U,P,L), hit(U,P,L,[])).

/***************************************************
* filtering
***************************************************/

%%	filter(+Hits, -Filtered, +Options)
%
%	Fitered contains only those hits that
%	satisfy the Filter from Options.

filter(Hits0, Hits1, Options) :-
	(	option(filter(Filter), Options),
		Filter \== []
	->	filter_hits(Hits0, Filter, Hits1)
	;	Hits1 = Hits0
	).
filter(Hits, Hits, _Options).


filter_hits([], _, []) :- !.
filter_hits(HitsIn, Filter, HitsOut) :-
	filter_to_goal(Filter, R, Goal),
	findall(Hit, (member(Hit, HitsIn),
		      Hit = hit(R,_,_,_),
		      once(Goal)),
		HitsOut).
