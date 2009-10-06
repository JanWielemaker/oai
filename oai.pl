:- module(oai,
	  [ oai_request/4,		% +Server, +Verb, :Handler, +Options
	    oai_server_address/2	% +Id, -URL
	  ]).
:- use_module(library('http/http_client')).
:- use_module(library('http/http_sgml_plugin')).
:- use_module(library(debug)).

:- meta_predicate oai_request(+, +, :, +).

:- multifile oai_server/2.
:- dynamic   oai_server/2.

oai_server(den,      'http://erfgoed.medialab.nl/OAI/Igor-oai.asp').
oai_server(igem,     'http://igem.adlibsoft.com/wwwopac.exe').
oai_server(citeseer, 'http://cs1.ist.psu.edu/cgi-bin/oai.cgi').
oai_server(fontys,   'http://www.fontyspublicaties.nl/oai/pub.fontys.nl.cgi').
oai_server(svcn,     'http://62.221.199.220:26006/').
oai_server(scran,    'http://www.scran.ac.uk/xmlrpc/oai/oai2.php').
oai_server(dismarc,  'http://www.dismarc.org/oai/index.php').


		 /*******************************
		 *	      ACTIONS		*
		 *******************************/

%%	oai_list_records(+Server, +Verb, :Handler, +Options)
%
%	Run Verb on Server. If the response is a list (all List* verbs),
%	Handler is called for each element in the response. Otherwise it
%	is  called  for  the  response  as  a  whole.  Options  provides
%	additional options for the OAI HTTP request.

oai_request(Server, Verb, Handler0, Options) :-
	strip_module(Handler0, Context, Goal),
	Handler = Context:Goal,
	oai_attributes(Options, Fields, RestOptions),
	make_url(Server, Verb, Fields, ParsedURL),
	request(ParsedURL, Verb, Handler, RestOptions).

request(ParsedURL, Verb, Handler, RestOptions0) :-
	(   select(resume(Resume), RestOptions0, RestOptions)
	->  true
	;   Resume = true,
	    RestOptions = RestOptions0
	),
	parse_url(FullURL, ParsedURL),
	debug(oai, 'Opening ~w ...', [FullURL]),
	http_get(ParsedURL, XML, [space(remove)|RestOptions]),
	debug(oai, 'Processing reply ...', []),
	(   Elem = element(_:Verb, _, _),
	    sub_term(Elem, XML)
	->  handle_content(Elem, Verb, Handler, ResumptionToken),
	    (	ResumptionToken \== [], Resume \== false
	    ->	resumption_url(ParsedURL, ResumptionToken, ResumeURL),
		request(ResumeURL, Verb, Handler, RestOptions)
	    ;	true
	    )
	;   sub_term(element(_:error, _, Error), XML)
	->  throw(error(oai(Verb, Error), _))
	;   throw(error(oai(unknown_reply), _))
	).

handle_content(Elem, Verb, Handler, ResumptionToken) :-
	oai_verb(Verb, ContentElem),
	ContentElem \== self, !,
	arg(3, Elem, Content),
	handle_content_parts(Content, ContentElem, Handler,
			     [], ResumptionToken).
handle_content(Elem, _, Handler, []) :-
	call(Handler, Elem).

handle_content_parts([], _, _, RT, RT).
handle_content_parts([Elem|T], E, Handler, RT0, RT) :-
	match_element(Elem, E), !,
	call(Handler, Elem),
	handle_content_parts(T, E, Handler, RT0, RT).
handle_content_parts([element(_:resumptionToken,_,[RT1])|T], E, H, _, RT) :- !,
	handle_content_parts(T, E, H, RT1, RT).
handle_content_parts([Error|T], E, H, RT0, RT) :-
	print_message(warning, oai(skipped(Error))),
	handle_content_parts(T, E, H, RT0, RT).

match_element(element(_:E, _, _), E) :- !.
match_element(element(E, _, _), E).


		 /*******************************
		 *	   URI HANDLING		*
		 *******************************/

%%	make_url(+Server, +Verb, +ExtraFields, -ParsedURL)

make_url(Server, Verb, Fields, All) :-
	oai_server_address(Server, BaseURL),
	parse_url(BaseURL, Parts),
	All = [ search([ verb = Verb
		       | Fields
		       ])
	      | Parts
	      ].

oai_server_address(Name, Server) :-
	oai_server(Name, Server), !.
oai_server_address(Server, Server) :-
	sub_atom(Server, 0, _, _, 'http'), !.
oai_server_address(Server, _) :-
	throw(error(existence_error(server, Server), _)).

%%	resumption_url(+Parsed, +ResumptionToken, -ResumptionURL)
%
%	Replace or add the resumptionToken argument of the URL.

resumption_url(ParsedURL, ResumptionToken, NewURL) :-
	select(search(Search0), ParsedURL, URL1),
%	(   select(resumptionToken(_), Search0, Search1)
%	->  Search = [resumptionToken(ResumptionToken)|Search1]
%	;   Search = [resumptionToken(ResumptionToken)|Search0]
%	),
	memberchk(verb=Verb, Search0),
	Search = [ resumptionToken = ResumptionToken,
		   verb = Verb
		 ],
	NewURL = [search(Search)|URL1].


		 /*******************************
		 *	     OPTIONS		*
		 *******************************/

%%	oai_attributes(+Options, -OAIArguments, -RestOptions
%
%	Split options in OAI  HTTP  request   arguments  and  the  rest.
%	OAIArguments is of the form   Name=Value, while RestOptions uses
%	the Name(Value) convention.

oai_attributes([], [], []).
oai_attributes([Name=Value|T0], OAI, Rest) :- !,
	Opt =.. [Name,Value],
	oai_attributes([Opt|T0], OAI, Rest).
oai_attributes([H|T0], [Name=Value|T], R) :-
	H =.. [Name,Value],
	oai_attribute(Name), !,
	oai_attributes(T0, T, R).
oai_attributes([H|T0], A, [H|T]) :-
	oai_attributes(T0, A, T).


%%	oai_attribute(?Name)
%
%	Enumerate the OAI attributes.

oai_attribute(from).
oai_attribute(until).
oai_attribute(set).
oai_attribute(resumptionToken).
oai_attribute(metadataPrefix).


%%	oai_verb(+Verb, -ContentPart)

oai_verb('GetRecord',		self).
oai_verb('Identify',		self).
oai_verb('ListIdentifiers',	identifier).
oai_verb('ListMetadataFormats',	metadataFormat).
oai_verb('ListRecords',		record).
oai_verb('ListSets',		set).



		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(oai(skipped(Element))) -->
	[ 'OAI: skipped ~p'-[Element] ].