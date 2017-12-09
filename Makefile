.SUFFIXES: .tex .pl .qof .spec .dvi .ps

.pl.qof:
	qpc -c $(LIBS) $<

.spec.tex :
	./src2latex $<

.pl.tex :
	./src2latex $<

.c.tex :
	./src2latex $<

.tex.dvi :
	latex $<

.dvi.ps :
	dvi2ps $< >$*.ps


PROGRAMS  = multiplex translate c_program
TMPFILES =  *.ps *.qof *.log *.to? *.dvi *.aux *~ pe *.o \
            user.tex multi.tex plex.tex c_program.tex

LIBS = -L .

PREAMBLE = "\documentstyle[11pt]{book}\hfuzz 10pt\newcommand{\predindex}[1]{\index{#1}} \begin{document} \input "
POSTAMBLE = "\end{document}"

all	: multiplex translate example2

mp_doc	: mp_doc.tex

plex_doc: plex.tex
	(set noglob; echo $(PREAMBLE) plex $(POSTAMBLE) | latex)

multi_doc: multi.tex
	echo $(PREAMBLE) multi $(POSTAMBLE) | latex

multiplex	:	multiplex.pl
	qpc -o multiplex $(LIBS) multiplex.pl

translate:	translate.pl multi.pl plex.pl
	qpc -o translate $(LIBS) translate.pl

c_program:	c_program.o multi.qof
	qld $(LIBS) -Dd multi.qof c_program.o -o c_program

example2:	example2.pl
	qpc -o example2 $(LIBS) example2.pl

clean	:
	/bin/rm -rf syn_*.pl $(PROGRAMS) $(TMPFILES) *.tmp







