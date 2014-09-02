:- module(acfilter,
	  [ target_goal/3,            % +Goal, +R, +Options
	    filter_to_goal/3	      % +FilterList, +R, -Goal
	  ]).

:- use_module(library(error)).

:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).
:- use_module(library('semweb/rdf_db')).

:- multifile
        cliopatria:in_scheme/2.  % filter hook

:- public
	acfilter:in_scheme/2.    % predicate calling hook above

%%	target_goal(+Goal, +R, -URI)
%
%	Succeeds if R passes all Filters.

target_goal(Goal, R, R) :-
	Goal, !.

%%	filter_to_goal(+Filter, +R, -Goal)
%
%	Goal is a prolog goal of Filter that succeeds if R passes all filters.
%       Allowed filters:
%
%	    * keyword(Keyword)
%	    * type(Class)
%	    * prop(Prop, Value)
%	    * reachable(Prop, TransitiveProp, Concept)
%
%	@see Options are described with rdfs_plus_skos/5

filter_to_goal(Filter, R, Goal) :-
	is_list(Filter), !,
	(   Filter = []
	->  Goal = true
	;   filter_to_conj_goal(Filter, R, Goal)
	).
filter_to_goal(Filter, R, Goal) :-
	filter(Filter, R, Goal).

%%	filter_to_conj_goal(+FilterList, +R, -Goal)
%
%	Goal is conjuctive prolog goal for FilterList.

filter_to_conj_goal([Filter], R, Goal) :- !,
	filter(Filter, R, Goal).
filter_to_conj_goal([Filter|T], R, (Goal,Rest)) :-
	filter(Filter, R, Goal),
	filter_to_conj_goal(T, R, Rest).

%%	filter_to_conj_goal(+FilterList, +R, -Goal)
%
%	Goal is disjunctive prolog goal for FilterList.

filter_to_disj_goal([Filter], R, Goal) :- !,
	filter(Filter, R, Goal).
filter_to_disj_goal([Filter0|T], R, (Goal;Rest)) :-
	rdf_global_term(Filter0, Filter),
	filter(Filter, R, Goal),
	filter_to_disj_goal(T, R, Rest).

%%	filter(+Filter, -Goal, ?R)
%
%	Goal is a prolog goal that succeeds if R passes Filter.

filter(or(Filter), R, Goal) :- !,
	filter_to_disj_goal(Filter, R, Goal).
filter(or(F1, F2), R, Goal) :- !,
	Goal = (G1 ; G2),
	filter_to_goal(F1, R, G1),
	filter_to_goal(F2, R, G2).
filter(type(Class), R, Goal) :- !,
	(   rdf_equal(Class,rdfs:'Resource')
	->  Goal = true
	;   rdf_equal(Type, rdf:type),
	    rdf_equal(SubClass, rdfs:subClassOf),
	    Goal = ( rdf(R,Type,C),
	             rdf_reachable(C,SubClass,Class)
		   )
	).
filter(scheme(Scheme), R, Goal) :- !,
	Goal = acfilter:in_scheme(R, Scheme).
filter(prop(P, V), R, Goal) :- !,
	(   P = all
	->  Goal = rdf_has(R, _, V)
	;   Goal = rdf_has(R, P, V)
	).
filter(reachable(TransP, C), R,	Goal) :- !,
	Goal = rdf_reachable(R, TransP, C, 10, _).
filter(reachable(P, TransP, C), R, Goal) :- !,
	Goal = ( rdf_has(R, P, V),
		 rdf_reachable(V, TransP, C)
	       ).
filter(value(V), R, Goal) :- !,
	Goal = rdf_has(R, _, V).
filter(valueOfProp(P), R, Goal) :- !,
	Goal = rdf_has(_, P, R).
filter(valueOfProp(P, Filter), R, Goal) :- !,
	Goal = ( rdf_has(S, P, R),
		 Rest
	       ),
	filter_to_goal(Filter, S, Rest).
filter(metadata(Class), R, Goal) :- !,
	rdf_equal(Type, rdf:type),
	rdf_equal(SubClass, rdfs:subClassOf),
	Goal = (  rdf(R, Type, C),
		  rdf_reachable(C, SubClass, Class)
	       ;  rdf_has(S,_,R),
		  rdf(S, Type, C),
		  rdf_reachable(C, SubClass, Class)
	       ).
filter(metaclass(MetaClass), R, Goal) :- !,
	rdf_equal(Type, rdf:type),
	rdf_equal(SubClass, rdfs:subClassOf),
	Goal = ( rdf(R, Type, C),
		 rdf_reachable(C, SubClass, Class),
		 rdf(Class, Type, MetaClass)
		).
filter(equal(R1), R, Goal) :- !,
	Goal = rdf_equal(R1, R).
filter(ns(Ns), R, Goal) :- !,
	Goal = (   ground(R+Ns)
	       ->  sub_atom(R, _, _, _, Ns)
	       ;   rdf_url_namespace(R, Ns)
	       ).
filter(alias(Alias), R,	Goal) :- !,
	Goal = rdf_global_id(Alias:_, R).
filter(group(P, [Value]), R, Goal) :- !,
	(	Value == other
	->	Goal = (\+ rdf_has(R, P, Value, _))
	;	Goal = iface_has(R, P, Value, _)
	).
filter(Filter, _, _) :-
	domain_error(filter, Filter).

in_scheme(Concept, Scheme) :-
	catch(cliopatria:in_scheme(Concept, Scheme), _, fail),
	!.
in_scheme(Concept, Scheme) :-
	rdf_equal(P, skos:inScheme),
	rdf(Concept, P, Scheme).

http:convert_parameter(json_filter, Atom, Term) :-
	atom_json_term(Atom, JSON, []),
	json_to_prolog(JSON, Term).

:- json_object
    or(or:_),
    type(type:atom),
    scheme(scheme:atom),
    metaclass(metaclass:atom),
    metadata(metadata:atom),
    metadataOf(metadataOf:atom),
    prop(prop:atom, object:_),
    prop(prop:atom, uri:atom),
    prop(prop:atom),
    value(object:atom),
    valueOfProp(valueOfProp:atom),
    reachable(reachable:atom, uri:atom),
    reachable(prop:atom, reachable:atom, uri:atom),
    literal(value:_) + [type=literal],
    type(type:atom, literal:atom),
    lang(lang:atom, literal:atom).
