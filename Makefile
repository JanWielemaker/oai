PL=	load.pl oai.pl oairdf.pl pltotex.pl run.pl

all:	README.pdf

README.pdf: README.tex
	pdflatex README.tex

README.tex: README $(PL)
	./pltotex.pl --stand_alone=true --text README run.pl

clean::
	rm -f *~
	rm -f README.{tex,out,log,idx,aux}

