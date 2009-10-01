:- module(t20_extensions, []).
:- use_module(triple20(rdf_rules)).
:- use_module(triple20(rdf_cache)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- begin_rules(display, part).

child_cache(R, Cache, rdf_part_node) :-
	rdf_has(oai:server, rdfs:range, Domain),
	rdf_has(R, rdf:type, Class),
	rdfs_subclass_of(Class, Domain),
	rdf_cache(lsorted(V), rdf_has(V, oai:server, R), Cache).
child_cache(R, Cache, Class) :-
	super::child_cache(R, Cache, Class).

:- end_rules.
