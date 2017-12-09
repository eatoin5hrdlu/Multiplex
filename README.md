# Multiplex

This directory contains the distribution of the 
MULTI/PLEX system. This includes a Prolog version of LEX
and a system (MULTI) for creating parsers and generators
from a single langauge specification. Also, there may be
sample files in various languages (note the suffixes) and
several sample grammars (any files with the suffix .spec).

The three files:

              plex.pl
              multi.pl
              multi_errors.pl

should be moved to the Quintus library, or into a directory
which the user can specify as a library directory. This 
would usually be somewhere like:

     /usr/local/quintus/generic/qplib3.2/library
               
This varies depending upon your installation directory
(/usr/local) and version number(3.2). Once these three
files have been installed in the library, the multiplex
utility can be compiled and placed into a suitable bin
directory.

% qpc -o multiplex multiplex.pl
% mv multiplex /usr/local/quintus/bin3.2/sun4-4.1/multiplex

The instructions and Makefile in this distribution assume
the presence of a Quintus runtime license, to allow the
separate compilation of stand-alone runtime executables.
If you do not have a runtime license, these steps will
require you to consult the files into the development
system and create a savestate with the "save_program(myprog)"
command. Then a shell script with the command:

   echo "runtime_entry(start)." | myprog

Should give the same result as a stand-alone executable.

A program (c_program.c) has been included as an
example of how to use the MULTI/PLEX system as a
library routine from a C program.


The rest of this description contains instructions for:

     A) HOW TO USE PLEX
       1) Creating a Tokenizer
       2) Creating a Tokenizer at Run-time
       3) Creating a Tokenizer at Compile-time

     B) HOW TO USE MULTI
       1) MULTI/PLEX Grammar Specifications
       2) Creating a Parser/Generator System
       3) Creating a Parser/Generator System at Run-time

     C) USING MULTI/PLEX FROM C-PROGRAMS

     D) OTHER USES OF MULTI/PLEX

-----------------------------------------------------
            A) HOW TO USE PLEX

             
            A.1) CREATING A TOKENIZER

   With the "spec" file below, the command:

        multiplex -lexer sample.spec

   will create the source code for a lexical analyzer:
     
        sample(+CharList, -TokenList)

   in the file:

        sample_plex.pl

-----------Begining of File sample.spec-----------------

sample lexicon

"start"                           is [start|R] if switch(sub,R);

"[0-9]+"                          is number(N)
                                  if (text(T), number_chars(N,T)) ;
"[0-9]+\.[0-9]+([eE]-?[00-9]+)?"  is number(N)
                                  if (text(T), float_chars(N,T));
"[A-Za-z][A-Za-z_0-9-]*"          is  id(ID)
                                  if (text(T), name(ID,T)) ;
"""[^""]*"""                      is  quote(T)
                                  if  text(T) ;
"[ \t\n]"                         is  T if switch(sample,T);
"."                               is  C
                                  if (text(T), name(C,T)).


sub lexicon

"atom"       is atomic;
"term"       is terminal;
"[ \t\n]*"   is T       if switch(sub, T);
"end"        is [end|T] if switch(sample, T).

-----------End of File sample.spec-----------------

Notice that PLEX does not have "STATES" the way LEX does.
Instead, one defines a number of lexicons (here we have
lexicons "sample" and "sub").  Any lexicon may switch/2
to another lexicon, including itself. This is good for
implementing sub-languages and ignoring white-space.

Notice that the "sub" lexicon can only recognize the
words {atom, term, end} and whitespace. A syntax error
will occur if anything else is encountered while the
lexer is in this "state".


      A.2) CREATING A LEXICAL ANALYZER AT RUN-TIME

It is possible to create a lexical analyzer at run-time
and immediately use it to tokenize character lists.
This is done by the following program which will read
the file sample.spec at runtime. 

Imagine a program EXAMPLE2 which is given a filename
as a command line argument and must construct a suitable
lexical analyzer based on the filename suffix.  

For example, if EXAMPLE2 is given the argument "data.sample",
it will assume that a language specification named "sample.spec"
is available. It will then construct the tokenizer with
the name "sample_parse:sample/2", and tokenize the contents
of the file "data.sample".

Notice that when a lexical analyser for language xxx is
built "on-the-fly" like this, it is put into its own
module: xxx_parse.

-----------Begining of File EXAMPLE2.pl-----------------
:- use_module(library(plex)).
:- use_module(library(strings), [concat_atom/2]).

runtime_entry(start) :-
     unix(argv([File])),                      % e.g. 'data.sample'
     name(File, FileChs),
     append(_, [0'.|LangChs], FileChs),
     name(Lang, LangChs),
     concat_atom([Lang,'.spec'], SpecFile),   % e.g. 'data.spec'
     concat_atom([Lang,'_parse'], Module),    % e.g  'data_parse'

     consult(SpecFile),           % BUILD LEXICAL ANALYZER

     Lexer =.. [Lang, Chs, Tks],              % e.g. data(+Chs,-Tks)

     see(File),
     repeat,
       (get_line(Chs) ->
               call(Module:Lexer), % CALL LEXICAL ANALYZER
               write(Tks),nl,
               fail
       ; true
       ).

get_line(Line) :-
   get0(C),
   get_line(C,Line).

get_line(-1,   _)  :- !, fail.
get_line(10,[10])  :- !.
get_line(C,[C|Cs]) :-
   get0(NC),
   get_line(NC,Cs).
-----------End of File EXAMPLE2.pl-----------------


     A.3 Creating a Tokenizer at Compile-time

It is also possible to have a lexical specification 
in with other Prolog source code and build the
lexical analyzer at compile time.  A version of the
EXAMPLE3 program which does this is:

---------Begining of File EXAMPLE3.pl--------------

:- load_files(plex, [when(both)]).

sample lexicon

"start"                           is [start|R] if switch(sub,R);

"[0-9]+"                          is number(N)
                                  if (text(T), number_chars(N,T)) ;
"[0-9]+\.[0-9]+([eE]-?[00-9]+)?"  is number(N)
                                  if (text(T), float_chars(N,T));
"[A-Za-z][A-Za-z_0-9-]*"          is  id(ID)
                                  if (text(T), name(ID,T)) ;
"""[^""]*"""                      is  quote(T)
                                  if  text(T) ;
"[ \t\n]"                         is  T if switch(sample,T);
"."                               is  C
                                  if (text(T), name(C,T)).


sub lexicon

"atom"       is atomic;
"term"       is termlish;
"[ \t\n]*"   is T if switch(sub, T);
"end"        is [end|T] if switch(sample, T).


runtime_entry(start) :-
     unix(argv([File])),                      % e.g. 'data.sample'
     see(File),
     repeat,
       (get_line(Chs) ->
               sample_parse:sample(Chs, Tks),
               write(Tks),nl,
               fail
       ; true
       ).

get_line(Line) :-
   get0(C),
   get_line(C,Line).

get_line(-1,   _)  :- !, fail.
get_line(10,[10])  :- !.
get_line(C,[C|Cs]) :-
   get0(NC),
   get_line(NC,Cs).

-----------End of File EXAMPLE3.pl-----------------

            B) HOW TO USE MULTI
      
     B.1) MULTI/PLEX Grammar Specifications

In addition to writing lexical specification, a programmer
can also write BNF-style rules which define a grammar
and have multiplex turn this into a parser and a generator
for this language. The parser will build a "parse-tree"
when reading the language and the printer will generate
the textual version of this language from this "parse-tree".

As the user writes a MULTI/PLEX grammar, they must include
calls which relate the data items of the language with
places in the parse-tree. The tree is currently very
simple, having only two types of structure. The parse
tree consists of lists of objects, where each object is
either a NAME:TYPE:VALUE triple, or a "subcell" hierarchy
designator. In otherwords, any language must be reduced
to a two-category system of objects and containers. Where
an object has an identifier (NAME), a TYPE, and a VALUE 
where these three elements can be any Prolog structures.

The user must provide these grammar rules with calls to 
MULTIPLEX database functions. The parser uses these functions
to place data into the parse-tree and the printer will use
them to extract the information for generation.
We have the {\tt update(s)//2,3} calls to access the internal
data structure, the hierarchy management commands 
{\tt up//0} and {\tt down//1}, and formatting commands:
{\tt newline//0}, {\tt indent//0}, and {\tt undent//0}.
A built-in programmable expression parser
({\tt expression//1}) is also included.

A complete MULTI/PLEX specification file is given below.
A complete Parser/Generator system can be constructed
from this specification, including a lexical analyzer
courtesy of PLEX.

-----------------Begin sample2.spec--------------

sample2 lexicon

"[0-9]+"                          is number(N)
                                  if (text(T), number_chars(N,T)) ;
"[0-9]+\.[0-9]+([eE]-?[00-9]+)?"  is number(N)
                                  if (text(T), float_chars(N,T));
"[A-Za-z][A-Za-z_0-9-]*"          is  id(ID)
                                  if (text(T), name(ID,T)) ;
"""[^""]*"""                      is  quote(T)
                                  if  text(T) ;
"[ \t\n]"                         is  T if switch(sample2,T);
"."                               is  C
                                  if (text(T), name(C,T)).


sample2  ::= file(name.sample, sample), cell.

cell ::= down(Name),
          cell(Name),
         up.

cell(Name) ::= [ Type ], [ '(' ], arguments(Params), [')'],
               [ begin,  Name ],
                 statements,
               [ end ], optional([Name]), [';'],
               update(cell_info,Type,Params).

statements ::= value_attribute, statements(Data).
statements ::= cell,            statements(Data).
statements ::= [].

value_attribute ::= [ Type, :, Name ], value(V), [ ';' ],
                    update(Type,Name,V).

value(Vs) ::= [ '(' ], arguments(Vs).
value(V)  ::= [ V ].

arguments([])     ::= [')'].
arguments(V)      ::= [V, ')'].
arguments([V|Vs]) ::= [V], more_values(Vs).

--------------End of File sample2.spec--------------

     B.2) Creating a Parser/Generator System

With the specification file above, the source-code for
a Parser/Generator system can be created by the command:

    multiplex -all sample2.spec

This creates three files:  sample2_parse.pl sample2_plex.pl sample2_print.pl

A program using the Parser/Generator can be compiled with
the command:

    qpc -o my_prog my_prog.pl sample2_parse.pl sample2_print.pl

Note that sample2_plex.pl does not have to be included on the
compile line -- it is automatically included by the parser.


      B.3  Creating Parsers/Generators at Run-time

Of course, the most powerful way to use the MULTI/PLEX
system is to build or load Lexers/Parsers/Generators
at run-time. This allows you to build tools which are
truly language-independent.  The following program is
a multi-language translator which selects a parser and
generator at run-time. This translator is built with
the command:
      
   qpc -o translate translate.pl

---------------Begining of File translate.pl-------------
:- use_module(library(strings), [concat_atom/2]).
:- use_module(library(multi)).

runtime_entry(start) :-
     prolog_flag(character_escapes, _, on),
     unix(argv(Args)),
     multiplex_set_options(Args, [In, Out]),
     multiplex_suffix(In, _, InLang),
     multiplex_input(In, Data),

     % Any algorithm to transform the
     % Data can go here.
 
     multiplex_output(Out, Data).

runtime_entry(start) :-
    format('Usage: translate ~a ~a ~a~n',
          [ '[-v]', 'input.lang1', 'output.lang2' ]).
    % -v is Verbose option to see what is going on

------------------End of File translate.pl---------------

Specification files for (tiny) subsets of German and Spanish
are given below.  A data file, "data.german" contains the
two subject phrases "der Hund" and "die Katze".
Thecommand:

     translate data.german data.spanish

This command will will read noun-phrases in German from
the file data.german and write them in Spanish into the
file data.spanish. It will translate the German phrases to:

     "el perro" and "el gato"

Notice that the translator gets the correct gender of
"cat" (gato, katze), even though it is different in the
two languages. In fact, the "internal representation" used
is English, which does not even represent the gender of nouns!
This is a good example of what information belongs in the
language specification file, and what information must 
be stored in the internal representation.

-----------Begining of File german.spec---------------

german lexicon

"[A-Za-z]+"  is Word  if (text(T),name(Word,T)) ;
"[ ,\t\n]"    is R     if switch(german,R).

german ::=
   file('name.german',german),
   subjeckt,
   opt_subjeckts.

opt_subjeckts ::= subjeckt, opt_subjeckts.
opt_subjeckts ::= [].

subjeckt ::= artikel(Gender), hauptwort(N,Gender),
             update(subject, N).

hauptwort(dog, mannlich) ::= [ 'Hund' ].
hauptwort(cat, weiblich) ::= [ 'Katze' ].

artikel(mannlich) ::= [ der ].
artikel(weiblich) ::= [ die ].
artikel(sachlich) ::= [ das ].

-----------End of File german.spec---------------

-------Begining of File spanish.spec-------------

spanish lexicon
"[a-z]+"    is Word  if (text(T), name(Word,T));
"[ ,\t\n]"  is R     if switch(spanish,R).

spanish ::=
    file('name.spanish',spanish),
    subjectivo,
    opt_subjectivos.

subjectivo ::= articulo(Gender), nombre(N,Gender),
               update(subject, N).

opt_subjectivos ::= subjectivo, opt_subjectivos.
opt_subjectivos ::= [].

nombre(dog, masculino) ::= [ perro ].
nombre(cat, masculino) ::= [ gato ].

articulo(masculino) ::= [ el ].
articulo(feminina)  ::= [ la ].

-----------End of File spanish.spec--------------


Now, with the newly created data file "data.spanish"
you can translate it back to german with:

    translate   data.spanish   newdata.german

Compare "newdata.german" with the original "data.german"

The second time you run trans on these languages, it
will run much faster. The first time, it had to build
the lexical analyzers and compile the parser and printer
code. After doing this, it saved the Parser/Generator
system in the files german.spec.qof and spanish.spec.qof.

From now on, these languages can be loaded in about a second
from these "QOF" files, although they are still external
to the translation program.

Any change to the specification files (german.spec or spanish.spec)
will cause trans to re-consult the language specification,
but if the spec files do not change, it will take the
shortcut of loading the qof files and performing
the translation immediately.

To translate from (or to) a new language, simply 
write a spec file for that language.  For example, given
a data file "data.newlanguage", you will require a spec
file named "newlanguage.spec". Then you can type the command:

    translate   data.newlanguage    data.fpdl

            -OR-

    translate   small.syn     small.newlanguage


         *****************************
         ******* DOCUMENTATION *******
         *****************************

Complete LaTeX source for the documentation for PLEX or MULTI/PLEX
can be created from the Prolog source files by the utility "src2latex".

    % src2latex plex.pl 
    % latex plex.tex

           ************************

          C.  USING MULTI/PLEX FROM C PROGRAMS

It is possible to call the MULTI/PLEX system from 
a C program and then operate (in C) on the resulting data.
However, before using MULTI/PLEX as a subroutine library
for a C program, I strongly recommend :
 
     "Logic Programming and Compiler Writing"
      by David H. D. Warren,
      in Software -- Practice and Experience
         Vol 10, Number 2, pp 97-125, 1980.

This paper explains why Prolog is such a good language
for manipulating parse-trees and building compiler-like
tools. If you still convinced that you need a C program,
see the file user.c.  To link MULTI/PLEX with the program:

  % cc -c c_program.c
  % qpc -c multi.pl
  % qld -Dd multi.qof c_program.o -o c_program
  
The executable "c_program" calls MULTI/PLEX with the
filename given as argument and then examines the Prolog
structure returned by multiplex_input/2.


           ************************

          D.  OTHER USES OF MULTI/PLEX


                  CAD Framework

After translation, the most obvious use of MULTI/PLEX is
to build language-independent front and back ends for
tools -- to a large extent, this is the essential function
of a CAD Framework.

Rather than running a translator on a Verilog file 
only to feed it into a VHDL compiler for simulation
or synthesis, we can build the compiler to accept
the "data structure" from MULTI/PLEX and make it
a language-independent compiler. This is no more (or less)
problematic than having a good translator, but it
eliminates several steps of reading and writing.


                   CASE Tools

High-level Hardware description languages such as VHDL
and Verilog are specialized programming languages. 
This suggests that MULTI/PLEX can be useful in any
application which requires input and output of formal
languages. Output of formal (e.g. programming) languages
further suggests the production of software for
different applications and environments. 

A menu-driven tool which will generate VHDL or Verilog
descriptions from schematic drawings is quite similar
to the new generation of CASE associated with the
"Software through Pictures" slogan.

To expedite work in this area, we are considering the
development of MULTI/PLEX grammars for ``C'' and ADA.


           Translation for Simulation

A portable simulation environment can be constructed by
a VHDL-to-C translation. This translation would require
quite a bit of supporting software in C. Using the CASE
methodology, even this supporting software could be
language independent, allowing the user to select between
``C'', ADA, or Pascal as a target language for the portable
simulation environment.


