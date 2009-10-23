:- module(oairdf,
	  [ oai_server_properties/2,	% +ServerID, +DB

	    oai_identify/2,		% +ServerID, +DB
	    oai_sets/2,			% +ServerID, +DB
	    oai_metadata/2,		% +ServerID, +DB

	    oai_records/3,		% +ServerID, +DB, +Options
	    oai_crawl/3,		% +ServerID, +File, +Options
	    oai_craw_by_set/3,		% +ServerID, +Dir, +Options

	    oai_reset_warnings/0
	  ]).
:- use_module(oai).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(broadcast)).

:- multifile
	rdf_db:ns/2.

rdf_db:ns(oai, 'http://www.openarchives.org/OAI/2.0/').

:- initialization
	rdf_load(ontology('rdfs')),
	rdf_load(ontology(dc)),
	rdf_load(ontology(dcterms)),
	rdf_load(ontology(dctypes)),
	rdf_load('oai.ttl').

/** <module> Fetch data from an OAI server into an RDF database


*/

%%	oai_server_properties(+ServerID, +Graph) is det.
%
%	Get information about the OAI server   ServerID  and store it in
%	the given named graph. This call   creates RDFS instances of the
%	following classes:
%
%	    * oai:Server
%	    * oai:Set
%	    * oai:metadataFormat
%
%	The datamodel is described in the file =|oai.ttl|=.

oai_server_properties(ServerID, DB) :-
	try(oai_identify(ServerID, DB)),
	try(oai_sets(ServerID, DB)),
	try(oai_metadata(ServerID, DB)),
	broadcast(triple20(refresh)).

:- meta_predicate
	try(0).

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

%%	oai_crawl(+Server, +File, +Options)
%
%	Collects all records from Server into File. Some useful options:
%
%	    * retry(+Times)
%	    Retry the fetch max Times.  Default is 200.
%
%	    * retry_delay(+Seconds)
%	    Time to wait for the first retry.  Default is 10 seconds.
%	    For subsequent retries, the time is doubled each try. until
%	    it reaches =retry_maxdelay=
%
%	    * retry_maxdelay(+Seconds)
%	    Maximum delay between retries.  Default is 3600.
%
%	    * resumption_count(+Count)
%	    Do at most Count resumptions.  Default is infinite.
%
%	    * metadataPrefix(+Prefix)
%	    Meta-data format to collect.  Default is =oai_dc=.
%
%	    * resumptionToken(+Token)
%	    Start from the given resumption token
%
%	    * set(+Set)
%	    Specify a dataset
%
%	    * from(+From)
%	    * until(+Until)
%	    FIXME: OAI attributes; what do they do?
%
%	This predicate use the RDF graph =oai_crawler= for storing
%	intermediate data.  This graph is filled and wiped for each
%	resumption-token.

oai_crawl(Server, File, Options) :-
	select_option(resumption_count(Count), Options, Options1, -1),
	setup_call_cleanup(open(File, write, Out, [encoding(utf8)]),
			   fetch_record_loop(Count, Server, Out, Options1),
			   close(Out)).

fetch_record_loop(0, _, _, _) :- !.
fetch_record_loop(Count, Server, Out, Options) :-
	rdf_retractall(_,_,_,oai_crawler),
	retry_oai_records(1, Server, oai_crawler,
			  [ next_resumption_token(NextToken)
			  | Options
			  ]),
	comment(Out, Options, Options1),
	rdf_save_turtle(Out, [ graph(oai_crawler) ]),
	flush_output(Out),
	(   NextToken == []
	->  true
	;   C2 is Count - 1,
	    fetch_record_loop(C2, Server, Out,
			      [ resumptionToken(NextToken)
			      | Options1
			      ])
	).


retry_oai_records(Try, Server, DB, Options) :-
	(   catch(oai_records(Server, DB, Options), E, true),
	    (   var(E)
	    ->	true
	    ;	report_error(E),
		fail
	    )
	->  true
	;   option(retry(MaxRetry), Options, 100),
	    Try < MaxRetry
	->  option(retry_delay(Delay0), Options, 10),
	    option(retry_maxdelay(MaxDelay), Options, 3600),
	    Delay is min(MaxDelay, Delay0 * (2**(Try-1))),
	    debug(oai, 'Retrying in ~D seconds ...', [Delay]),
	    sleep(Delay),
	    Retry is Try + 1,
	    retry_oai_records(Retry, Server, DB, Options)
	).


report_error(E) :-
	print_message(error, E),
	open('oai-crawl-errors.txt', append, Out, [encoding(utf8)]),
	get_time(Now),
	format_time(string(Date), '%+', Now),
	format(Out, '~N~n% ~s~n~q.~n', [Date, E]),
	close(Out).


comment(Out, Options0, Options) :-
	get_time(Now),
	format_time(Out, '# Downloaded: %+\n', Now),
	(   select_option(resumptionToken(Token), Options0, Options)
	->  format(Out,  '# OAI resumptionToken: ~w~n~n', [Token])
	;   Options = Options0
	).


%%	oai_craw_by_set(+ServerId, +Dir, +Options)
%
%	Crawl a server  set-by-set,  where  each   set  creates  a  file
%	<set>.ttl  in  the  directory   Dir.    Options   is  passed  to
%	oai_crawl/3. In addition, we process the following:
%
%	    * if(not_exists)
%	    If provided and the download file already exists, the set
%	    is skipped.

oai_craw_by_set(ServerID, Dir, Options) :-
	ensure_directory(Dir),
	oai_sets(ServerID, ServerID),
	forall(rdf(Set, rdf:type, oai:'Set', ServerID),
	       crawl_set(ServerID, Dir, Set, Options)).

ensure_directory(Dir) :-
	exists_directory(Dir), !.
ensure_directory(Dir) :-
	make_directory(Dir).

crawl_set(ServerID, Dir, Set, Options) :-
	atomic_list_concat([Dir, /, Set, '.ttl'], File),
	(   option(if(not_exists), Options),
	    exists_file(File)
	->  print_message(informational, oai(skipped(exists, Set)))
	;   oai_crawl(ServerID, File, [set(Set)|Options])
	).


%%	oai_records(+ServerId, +Graph, +Options)
%
%	Fetch OAI records from ServerId into   the  given named Graph of
%	the RDF database.

oai_records(ServerId, DB, Options0) :-
	merge_options(Options0, [ metadataPrefix(oai_dc) ], Options),
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
make_value(_, Content, XMLLiteral, literal(type(XMLLiteral, Content)), _) :-
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

%%	oai_reset_warnings
%
%	Reset warnings that are already given

oai_reset_warnings :-
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
	prolog:message//1.

prolog:message(xmlrdf(no_range(Prop, Assumed))) -->
	[ 'XMLRDF: No range for property ~p; Assuming ~p'-[Prop, Assumed] ].
prolog:message(oai(skipped(exists, Set))) -->
	[ 'OAI Crawler: skipped set "~w": file exists'-[Set] ].
