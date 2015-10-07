:- module(oai_servers,
	  [ oai_server/2
	  ]).

%%	oai_server(+ServerID, -URL) is multi.
%
%	Defined short-names for OAI servers.  Please add rules to this
%	predicate to define your own servers.

oai_server(den,      'http://erfgoed.medialab.nl/OAI/Igor-oai.asp').
oai_server(igem,     'http://igem.adlibsoft.com/wwwopac.exe').
oai_server(citeseer, 'http://cs1.ist.psu.edu/cgi-bin/oai.cgi').
oai_server(fontys,   'http://www.fontyspublicaties.nl/oai/pub.fontys.nl.cgi').
oai_server(svcn,     'http://62.221.199.220:26006/').
oai_server(scran,    'http://www.scran.ac.uk/xmlrpc/oai/oai2.php').
oai_server(dismarc,  'http://www.dismarc.org/oai/index.php').
oai_server(stabi,    'http://digital.staatsbibliothek-berlin.de/oai/').
oai_server(beng,     'http://openskos.beeldengeluid.nl/oai-pmh').
oai_server(openskos, 'http://openskos.org/oai-pmh').
oai_server(rma,	     'https://www.rijksmuseum.nl/api2/oai/8zmxuaJ2').
