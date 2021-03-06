:- module(oai,
	  [ oai_request/4,		% +Server, +Verb, :Handler, +Options
	    oai_server_address/2	% +Id, -URL
	  ]).
:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(occurs)).
:- use_module(servers).

:- meta_predicate oai_request(+, +, 1, +).


		 /*******************************
		 *	      ACTIONS		*
		 *******************************/

%%	oai_list_records(+Server, +Verb, :Handler, +Options)
%
%	Run Verb on Server. If the response is a list (all List* verbs),
%	Handler is called for each element in the response. Otherwise it
%	is  called  for  the  response  as  a  whole.  Options  provides
%	additional options for the OAI HTTP request.  Other options:
%
%	    * resume(+Boolean)
%	    If =true= (default), continue if a resumption token is
%	    returned.
%
%	    * next_resumption_token(-Token)
%	    If provided, unify the resumptionToken with Token.  This
%	    option is mutual exclusive with the resume(Boolean) option.

oai_request(Server, Verb, Handler, Options) :-
	oai_attributes(Options, Fields, RestOptions),
	make_url(Server, Verb, Fields, URL),
	request(URL, Verb, Handler, RestOptions).

request(URL, Verb, Handler, Options0) :-
	(   select_option(next_resumption_token(Token), Options0, Options)
	->  ReturnToken = true
	;   select_option(resume(Resume), Options0, Options, true)
	),
	debug(oai, 'Opening ~w ...', [URL]),
	setup_call_cleanup(
		http_open(URL, In, Options),
		load_xml(In, XML, [space(remove),dialect(xmlns)]),
		close(In)),
	debug(oai, 'Processing reply ...', []),
	(   Elem = element(_:Verb, _, _),
	    sub_term(Elem, XML)
	->  handle_content(Elem, Verb, Handler, ResumptionToken),
	    (	ReturnToken == true
	    ->	debug(oai, 'oai_request/4: next token = ~q', [ResumptionToken]),
	        Token = ResumptionToken
	    ;	ResumptionToken \== [], Resume \== false
	    ->	resumption_url(URL, ResumptionToken, ResumeURL),
		request(ResumeURL, Verb, Handler, Options)
	    ;	true
	    )
	;   sub_term(element(_:error, _, Error), XML)
	->  throw(error(oai(Verb, Error), _))
	;   throw(error(oai(unknown_reply, XML), _))
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
handle_content_parts([element(_:resumptionToken,RA,RC)|T], E, H, _, RT) :-
	resumption_token(RA, RC, RT1), !,
	handle_content_parts(T, E, H, RT1, RT).
handle_content_parts([Error|T], E, H, RT0, RT) :-
	print_message(warning, oai(skipped(Error))),
	handle_content_parts(T, E, H, RT0, RT).

:- dynamic
	token/3.				% Token, Attrs, Time

resumption_token(Attrs, Tokens, _) :-
	debug(oai, 'Read resumptiontoken: ~q ~q', [Attrs, Tokens]),
	Tokens = [Token],
	get_time(Time),
	(   token(Token, Attrs0, Time0)
	->  debug(oai(duplicate),
		  'Duplicate token: ~q (Attrs = ~q/~q, Time = ~q/~q',
		  [ Token, Attrs0, Attrs, Time0, Time ])
	;   true
	),
	assertz(token(Token, Attrs, Time)),
	fail.					% proceed to next clause.
resumption_token(_, [Token], Token).
resumption_token(_, [], []).

match_element(element(_:E, _, _), E) :- !.
match_element(element(E, _, _), E).


		 /*******************************
		 *	   URI HANDLING		*
		 *******************************/

%%	make_url(+Server, +Verb, +ExtraFields, -ParsedURL)

make_url(Server, Verb, Fields, URL) :-
	oai_server_address(Server, BaseURL),
	parse_url(BaseURL, Parts),
	All = [ search([ verb = Verb
		       | Fields
		       ])
	      | Parts
	      ],
	parse_url(URL, All).

oai_server_address(Name, Server) :-
	oai_server(Name, Server), !.
oai_server_address(Server, Server) :-
	sub_atom(Server, 0, _, _, 'http://'), !.
oai_server_address(Server, Server) :-
	sub_atom(Server, 0, _, _, 'https://'), !.
oai_server_address(Server, _) :-
	throw(error(existence_error(server, Server), _)).

%%	resumption_url(+Parsed, +ResumptionToken, -ResumptionURL)
%
%	Replace or add the resumptionToken argument of the URL.

resumption_url(URL, ResumptionToken, NewURL) :-
	parse_url(URL, ParsedURL),
	selectchk(search(Search0), ParsedURL, URL1),
	memberchk(verb=Verb, Search0),
	Search = [ resumptionToken = ResumptionToken,
		   verb = Verb
		 ],
	parse_url(NewURL, [search(Search)|URL1]).


		 /*******************************
		 *	     OPTIONS		*
		 *******************************/

%%	oai_attributes(+Options, -OAIArguments, -RestOptions)
%
%	Split options in OAI  HTTP  request   arguments  and  the  rest.
%	OAIArguments is of the form   Name=Value, while RestOptions uses
%	the Name(Value) convention.
%
%	Note that if a resumptionToken  attribute   is  given, we cannot
%	give any other attributes according to the OAI specs.

oai_attributes(Options, OAI, RestOptions) :-
	split_attributes(Options, OAI0, RestOptions),
	(   memberchk(resumptionToken = Token, OAI0)
	->  OAI = [resumptionToken(Token)]
	;   OAI = OAI0
	).

split_attributes([], [], []).
split_attributes([Name=Value|T0], OAI, Rest) :- !,
	Opt =.. [Name,Value],
	split_attributes([Opt|T0], OAI, Rest).
split_attributes([H|T0], [Name=Value|T], R) :-
	H =.. [Name,Value],
	oai_attribute(Name), !,
	split_attributes(T0, T, R).
split_attributes([H|T0], A, [H|T]) :-
	split_attributes(T0, A, T).


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
