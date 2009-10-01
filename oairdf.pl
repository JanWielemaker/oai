:- module(oairdf,
	  [ oai_server_properties/2,	% +ServerID, +DB

	    oai_identify/2,		% +ServerID, +DB
	    oai_sets/2,			% +ServerID, +DB
	    oai_metadata/2,		% +ServerID, +DB

	    oai_records/3		% +ServerID, +DB, +Options
	  ]).
:- use_module(oai).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library(debug)).

:- multifile
	rdf_db:ns/2.

rdf_db:ns(oai, 'http://www.openarchives.org/OAI/2.0/').

:- initialization
	rdf_load(ontology('rdfs')),
	rdf_load(ontology(dc)),
	rdf_load(ontology(dcterms)),
	rdf_load(ontology(dctypes)),
	rdf_load('oai.rdfs').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of OAI server into an RDF database


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	oai_server_properties(+ServerID, +DB)
%	
%	Get information about the server

oai_server_properties(ServerID, DB) :-
	reset_warnings,
	try(oai_identify(ServerID, DB)),
	try(oai_sets(ServerID, DB)),
	try(oai_metadata(ServerID, DB)),
	broadcast(triple20(refresh)).

try(G) :-
	(   catch(G, E, true)
	->  (   var(E)
	    ->  true
	    ;   print_message(error, E)
	    )
	;   print_message(error, failed(G))
	).


oai_identify(ServerId, DB) :-
	oai_server_address(ServerId, ServerURL),
	oai_request(ServerURL, 'Identify', on_identify(ServerURL, DB), []).

on_identify(ServerURL, DB, XML) :-
	rdf_assert(ServerURL, rdf:type, oai:'Server', DB),
	set_properties(ServerURL, XML, DB).

oai_sets(ServerId, DB) :-
	oai_server_address(ServerId, ServerURL),
	oai_request(ServerURL, 'ListSets', on_set(ServerURL, DB), []).

on_set(ServerURL, DB, XML) :-
%	pp(XML),
	rdf_bnode(Set),
	rdf_assert(Set, rdf:type, oai:'Set', DB),
	rdf_assert(Set, oai:server, ServerURL, DB),
	set_properties(Set, XML, DB).

oai_metadata(ServerId, DB) :-
	oai_server_address(ServerId, ServerURL),
	oai_request(ServerURL, 'ListMetadataFormats',
		    on_metadata(ServerURL, DB), []).

on_metadata(ServerURL, DB, XML) :-
%	pp(XML),
	rdf_bnode(MDF),
	rdf_assert(MDF, rdf:type, oai:'metadataFormat', DB),
	rdf_assert(MDF, oai:server, ServerURL, DB),
	set_properties(MDF, XML, DB).

oai_records(ServerId, DB, Options) :-
	reset_warnings,
	oai_server_address(ServerId, ServerURL),
	oai_request(ServerURL, 'ListRecords',
		    on_record(ServerURL, DB), Options),
	broadcast(triple20(refresh)).

on_record(ServerURL, DB,
	  element(_, _, [ element(_:header, _HdrAtt, HDR)
			| Content
			])) :-
	rdf_bnode(URL),
	memberchk(element(_:identifier, _, [ID]), HDR),
%	debug(oai, 'Creating record ~w', [URL]),
	rdf_assert(URL, rdf:type, oai:'Record', DB),
	rdf_assert(URL, oai:server, ServerURL, DB),
	rdf_assert(URL, oai:identifier, literal(ID), DB),
	(   memberchk(element(_:datestamp, _, [Date]), HDR)
	->  rdf_assert(URL, oai:datestamp, literal(Date), DB)
	;   true
	),
	forall(element(metadata, Content, element(_, _, [MD])),
	       set_properties(URL, MD, DB)).
	
element(Name, Content, Element) :-
	member(Element, Content),
	match_element(Element, Name).

match_element(element(_:Name, _, _), Name) :- !.
match_element(element(Name, _, _), Name).


		 /*******************************
		 *	     PROPERTIES		*
		 *******************************/

set_properties(URL, element(_, Attrs, Content), DB) :-
	setp_from_attributes(Attrs, URL, DB),
	setp_from_content(Content, URL, DB).

setp_from_attributes([], _, _).
setp_from_attributes([AttName=Value|T], URL, DB) :-
	to_atom(AttName, Prop0),
	map_property(URL, Prop0, Prop, _Type),
	rdf_assert(URL, Prop, literal(Value), DB),
	setp_from_attributes(T, URL, DB).

setp_from_content([], _, _).
setp_from_content([element(EName, AL, CL)|T], URL, DB) :-
	to_atom(EName, Prop0),
	map_property(URL, Prop0, Prop, Type),
	make_value(AL, CL, Type, Value, DB),
	rdf_assert(URL, Prop, Value, DB),
	setp_from_content(T, URL, DB).


make_value([], [Value], Literal, literal(Value), _) :-
	rdf_equal(rdfs:'Literal', Literal), !.
make_value([xml:lang=Lang], [Value], Literal, literal(lang(Lang, Value)), _) :-
	rdf_equal(rdfs:'Literal', Literal), !.
make_value(_, Content, XMLLiteral, literal(Content), _) :-
	rdf_equal(rdfs:'XMLLiteral', XMLLiteral), !.
make_value(Attrs, Content, Type, ValueURL, DB) :-
	rdf_bnode(ValueURL),
	rdf_assert(ValueURL, rdf:type, Type, DB),
	setp_from_attributes(Attrs, ValueURL, DB),
	setp_from_content(Content, ValueURL, DB).


		 /*******************************
		 *	       MAP		*
		 *******************************/

:- dynamic
	warned_prop/1.

map_property(_Subject, Prop, Prop, Type) :-
	rdf(Prop, rdfs:range, Type), !.
map_property(_Subject, Prop, Prop, Literal) :-
	rdf_equal(rdfs:'Literal', Literal),
	(   warned_prop(Prop)
	->  true
	;   print_message(warning, xmlrdf(no_range(Prop, Literal))),
	    assert(warned_prop(Prop))
	).

reset_warnings :-
	retractall(warned_prop(_)).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

to_atom(NS:Value, Resource) :- !,
	atom_concat(NS, Value, Resource).
to_atom(Resource, Resource) :-
	atom(Resource).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(xmlrdf(no_range(Prop, Assumed))) -->
	[ 'XMLRDF: No range for property ~p; Assuming ~p'-[Prop, Assumed] ].
