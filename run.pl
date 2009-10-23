#!/staff/jan/bin/pl -L0 -G0 -T0 -s

:- [load].

:- debug(oai).

%%	sp(+ServerID) is det.
%
%	Fetch server properties and store them into an RDF graph named
%	ServerID.

sp(Server) :-
	oai_server_properties(Server, Server).

%%	gr(+ServerID) is det.
%
%	Fetch all records for ServerID and store them in the named graph
%	ServerID. This works for  relatively   small  repositories.  For
%	*big* repositories, see oai_crawl/3.

gr(Server) :-
	oai_records(Server, Server,
		    [
		    ]).

%%	gr1(+ServerID) is det.
%
%	As gr/1, but does not resume.

gr1(Server) :-
	oai_records(Server, Server,
		    [ resume(false)
		    ]).
