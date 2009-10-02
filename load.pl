file_search_path(triple20, '/home/janw/src/eculture/triple20/src').
file_search_path(triple20, '/soft/swi-prolog-dev/triple20/src').


:- load_files([ triple20(load),		% Use Triple20 for vizualisation
					% Get this in 'user'
		library(semweb/rdf_portray),
		library('semweb/rdf_db'),
		library('semweb/rdfs'),
					% The real stuff
		oai,
		oairdf,
					% Triple20 extensions
		t20_extensions		% after declaration of oai namespace!
	      ],
	      [ silent(true)
	      ]).

%	Triple20 setup

:- rdf_label_rules:view_label_as(label_only).
