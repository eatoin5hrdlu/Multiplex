:- module(multi,   [ multiplex_input/2,
                     multiplex_output/2,
                     multiplex_build/1,
                     multiplex_quit/0,
                     multiplex_set_options/2,
                     multiplex_suffix/3 ]).

:- op(1200, xfx, '::=').

:- extern(multiplex_input(+string, -term)).
:- extern(multiplex_output(+string, +term)).
:- extern(multiplex_build(+string)).
:- extern(multiplex_quit).
:- extern(multiplex_set_options(+term, -term)).
:- extern(multiplex_suffix(+string, -string, -string)).

:- use_module(plex).
:- use_module(library(strings),  [gensym/2, concat_atom/2]).
:- use_module(library(basics),   [member/2, memberchk/2]).
:- use_module(library(files),    [can_open_file/3, file_exists/2]).
:- use_module(library(directory),[file_property/3]).
:- use_module(library(ctypes)). 

:- dynamic  language/1, option/2, current_name/1, commit/0,
   lexer/0, parser/0, printer/0, language_loaded/1, open_stream/2,
   verbose/0, macro/3.
:- multifile multi:language/1, multi:language_loaded/1.
:- ensure_loaded(multi_errors).
/**
\long\def\code#1{}
\def\endcode{}
\chapter{MULTI: Generating Parsers and Printers}
\section{Introduction}
This chapter describes MULTI, a software component for
automatically constructing parsers and generators from 
a single high-level BNF language specification.
Together with PLEX, a lexical analyzer generator for
Prolog, this forms the MULTI/PLEX system for rapid 
development of translators and front- and back-ends
for CAD tools.

MULTIPLEX frees the translator developer to concentrate
on the issues of semantic equivalence by automating
the construction of tokenizers, parsers, and printers
and manages the common data structures.
These tasks are accomplished by dynamically transforming
language specifications into Prolog programs and
executing them. Unlike previous compiler-compilers MULTI/PLEX
does not require intermediate compilation steps. 
New parsers and generators can be constructed and executed
at runtime from user modifiable language specifications.

\subsection{Motivation}

Many engineering specification and data languages
have been created in recent years.
Such proliferation is probably a necessary characteristic
of such a fertile period of advancing technology,
but it requires a massive software development effort
to build the necessary translators.  The magnitude of
this effort goes largely unnoticed because it is
considered to be a minor part of a CAD
environment. While each individual
translator may not seem important, the number of 
such translators is quite large. There are
several reasons why CAD software organizations
have not taken a general approach to this problem:

\begin{enumerate}
\item{}
A general translation capability would make it
easy for customers to defect to other tool sets.
\item{}
It is cheaper in the short term to create only
the specific translator needed at the moment.
\item{}
A widely varying range of quality is required from these
translators. In some cases, a simple hack which only
works on certain data files may be sufficient.
\item{}
Usually only one direction of translation is neededn
for a given pair of languages.
\end{enumerate}

Previous work has shown that Prolog is a good language
for developing multi-lingual translators [Reintjes87].
Unfortunately, adding a new language in this previous
translation system required work by an experienced Prolog
programmer.  Even the relatively simple task of writing an
efficient tokenizer requires the programmer to consider
such issues as indexing, determinism, the efficiency of
arithmetic tests and non-backtrackable I/O.
The lack of widespread literacy in logic programming
means that few programmers are available to work
on such systems. This system both acknowledges this
lack of literacy and works toward eliminating it.

While this translator does not require its
users to become proficient in Prolog, it
does give them a logical language
in which to specify the information about the
languages to be translated.  This specification
language is a superset of Prolog in the
same way that YACC and LEX are supersets of C.
That is, most of the difficult aspects of lexical
and syntax analysis are handled at a high level, 
but the native language, in this case Prolog,
is available for general programming.

Figure 1 shows how MULTIPLEX performs a translation
by using the information in language specification
files.

\def\boxone#1{
  \put(19,30){\line(1,0){67}}     % horiz top
  \put(19,0){\line(1,0){67}}      % horiz bottom
  \put(20,0){\line(0,1){30}}      % vert left
  \put(85,0){\line(0,1){30}}      % vert right
  \put(25,15){#1}
}
\def\boxtwo#1#2{
  \put(19,30){\line(1,0){80}}     % horiz top
  \put(19,0){\line(1,0){80}}      % horiz bottom
  \put(20,0){\line(0,1){30}}      % vert left
  \put(98,0){\line(0,1){30}}      % vert right
  \put(25,17){#1}
  \put(25,7){#2}
}
\def\circleone#1#2{\circle{#1}\put(-17,-4){#2}}
\def\circletwo#1#2#3{\circle{#1}\put(-17,0){#2}\put(-15,20){#3}}

\picture(200,150)(-70,0)

\put(90,90){\vector(0,-1){15}}
\put(90,30){\vector(0,1){15}}

\put(20,60){\vector(1,0){30}}
\put(130,60){\vector(1,0){30}}
\linethickness{2pt}
\put(0,60){\circleone{40}{Data A}}
\put(30,90){\boxtwo{\ Spec File}{\ Language A}}
\put(30,45){\boxtwo{MULTIPLEX}{\ \ Translator}}
\put(30,0){\boxtwo{\ Spec File}{\ Language B}}

\put(180,60){\circleone{40}{Data B}}

\endpicture
\vskip 1.0cm
\centerline{\bf Figure 1. MULTIPLEX Translation Data Flow}
\vskip 0.2cm

While MULTIPLEX can streamline the process of building
tokenizers, parsers, and generators in Prolog, the
architecture of this program was only partially
motivated by software engineering considerations.
We are also fulfilling a legal requirement by
decoupling the language description from the body
of the translation system. This program was developed
to translate circuit synthesis library models from
one vendor to another.  Unfortunately, each of these
vendors consider the syntactic details of their
language to be proprietary information.
Thus, if our translation program contained
specific information about these languages,
we could not distribute the program without
a myriad of licensing (and presumably, royalty) agreements.
In short, for legal purposes, the translation program
cannot embody any specific information about the languages
it is to translate.  Stated bluntly, this might
be enough to discourage anyone from undertaking such a project.
However, this restriction provided the needed
motivation to design MULTIPLEX as a kind of
``translation synthesizer''.

Therefore, not only are there not be enough Prolog programmers
to produce front- and back-ends for all of the necessary
languages, but it would literally be against the law to
sell a translator that was so constructed.  Once we had the
requirement that the language-specific information
be external to the program, this led naturally
toward efforts to make these specifications as concise
and simple as possible.

\subsection{MULTIPLEX BNF}

We refer to the MULTIPLEX specification language as BNF
because of its similarity with this standard form.
Prolog DCG's and YACC syntax are characterized
by the way they hide the list of tokens being parsed.
We go one step further and hide the parse-tree as well,
thus making our language a purer form of syntax
for which the Backus-Naur (nee Normal) Form has
become a standard notation.  We stray from pure BNF
with a single built-in goal which relates
the information in the syntax to the ``hidden'' parse-tree
(actually, there are many other built-in functions that
the user can employ, such as those for syntax errors, to
stray ever farther from pure BNF).

A MULTIPLEX specification consists of one or more lexical
specifications (called lexicons) which look suspiciously
like rules for the UNIX LEX utility [Lesk75]. The primary
difference is that the actions are written in
Prolog rather than ``C''.  We allow multiple lexicons
because of the multi-level nature of many languages
(discussed in section 4.3) and also because a single grammar
might cover data from a number of
different files, each with different lexical conventions.

The lexical specification is followed by the grammar
rules which take the form of BNF with special calls
which relate the data in the language to an internal structure.
These calls will either insert or
extract information from the underlying data structure
depending upon whether the grammar is being used to parse
or generate a description.

Because we wish to parse and generate languages from a single
specification, cuts and if-then-else structures are noticeably absent.

\subsection{Comparison with LEX, YACC and AWK}

LEX, YACC, and AWK are three of the primary language processing
tools associated with UNIX systems. LEX and YACC are universally
used for building front-ends for complex language processing
programs, such as compilers, while AWK is frequently
used for simple translation and filtering tasks.

AWK is usually described as a user-programmable filter and 
LEX and YACC as tools to build compilers. But filters and
compilers simply define two ends of the spectrum of translators.
Even GREP can described as a translator which converts a
file containing
general patterns of text, to a specific subset of that file.
In this spectrum of functionality from simple filters to
compilers these programs perform many of the same tasks.
AWK, with its fixed lexical analyzer, might 
be thought of as less powerful than tools
built with LEX and YACC.
At the same time it is more convenient to use because it
eliminates the intermediate compilation steps.

MULTIPLEX implements the general aspects of LEX and YACC
while having the dynamic characteristics of AWK.  In addition,
its YACC-like grammar definition specifies a generator (or 
pretty-printer) for each language as well as a parser.
It thus improves upon its predecessors in the following ways:

\begin{itemize}
\item{}
It eliminates intermediate file and compilation steps by
providing an end-user language which specifies
and drives the translation task.
\item{}
Lexical and grammatical information are combined
in a single specification.
\item{}
It constructs generators as well as parsers from
the grammatical specifications.
\item{}
It allows any number of lexical analyzers and parsers to
coexist in a single application.
\end{itemize}
\centerline{\bf Figure 2. MULTIPLEX Characteristics}
\vskip 0.2cm

\section{The MULTIPLEX Program}
The specification file contains the information necessary
to read data in a specific format.
The definitions in this file can express all of the
ways in which such library models might be different.

The translation system itself provides a simple
data structure which serves as a repository for the
information which is common to these languages. Since
we are dealing with Hardware Description languages,
this includes the general concepts of delay constraints,
combinatorial, and sequential machine descriptions.

The specification file and the MULTI/PLEX program thus
interact on three fronts. First, lexical definitons
in the specification file define the tokenizers to
scan the input data file. Second, the BNF-style
grammar rules in the specification define the
file access and grammatical structure of the data
and relate this to the internal data structure.

More detail on PLEX, the lexical analyzer generator
can be found in the PLEX documentation.

\subsection{The BNF Grammar Rules}

The user writes MULTI/PLEX grammar rules to 
define the language and relate the
data objects to generic data-types which will be
common to all such inter-translatable languages.

These grammar rules frequently contain calls to 
MULTI/PLEX built-in functions. In addition to
{\tt update//1} which accesses the internal
data structure, we have hierarchy management
commands {\tt up//0} and {\tt down//1} and the
formatting commands: {\tt newline//0}, {\tt indent//0},
and {\tt undent//0}. A built-in programmable expression
parser ({\tt expression//1}) is also built into MULTI/PLEX.

An example set of grammar rules is given below:
\begin{verbatim}
lang_file  ::= file(name.xyz,xyz), cell.

cell ::= down(SubTree),
          cell(Name),
         up,
         update(subcell(Name,Type,SubTree).

cell(Name) ::= [ Type ], [ '(' ], arguments(Params), [')'],
               [ begin,  Name ],
                 statements,
               [ end ], optional([Name]), [';'],
               update(cell_info(Name,Type,Params)).

statements ::= value_attribute, statements(Data).
statements ::= cell,            statements(Data).
statements ::= [].

value_attribute ::= [ Type, :, Name ], value(V), [ ';' ],
                    update(value_attribute(Name, Type, V)).

value(Vs) ::= [ '(' ], arguments(Vs).
value(V)  ::= [ V ].

arguments([])     ::= [')'].
arguments(V)      ::= [V, ')'].
arguments([V|Vs]) ::= [V], more_values(Vs).
\end{verbatim}

As an example of these grammar transformations
the {\tt more\_values//1} rule is split into
predictive parser and text generator versions,
both of which are completely deterministic.

\begin{verbatim}
in_more_values(Vs) --> [X], in_more_values1(X,Vs).

in_more_values1(',',[V|Vs]) --> [V], in_more_values(Vs).
in_more_values1(')',[])     --> [].
\end{verbatim}

\begin{verbatim}
out_more_values([])     --> ")".
out_more_values([V|Vs]) --> [0',|Cs],
                            { my_name(V,Cs) },
                            out_more_values(Vs).
\end{verbatim}

\subsection{Internal Data Representation}
The {\tt update//1} routines in the grammar specification
are used to insert or extract specific bits in the
data structure representing the file to be translated.
Since we do not want to distinguish between lookup and
and assignment, we have a dictionary similar to that
described by [Warren80].

The hierarchical capabilities of this generalized ``dictionary''
function allows the user to manage complex ``scoping''
information along with the basic data elements, all
without requiring direct manipulations of the Prolog structures.

The ``built-ins'' to support this access are
{\tt update//1} and {\tt examine//1}.

\subsection{Term Expansion}

The BNF grammar rules are transformed by {\tt term\_expansion/2} rules.
Thus, our parsing and printing grammars are constructed
for us by {\tt consult/1}.
Rather than re-inventing the DCG term-expansion,
we create two DCG-style clauses from our format
and call the normal {\tt expand\_term/2} predicate to
convert the DCG to native Prolog.
**/
multiplex_modules(Parse, Print) :-
     language(Lang),
     concat_atom([Lang,'_parse'], Parse),
     concat_atom([Lang,'_print'], Print).

normal_expansion(Clause, Clauses) :-
      multiplex_modules(P1Mod, P2Mod),
      expand_term(P1Mod:Clause, P1),
      expand_term(P2Mod:Clause, P2),
     ( contains_goal(Clause,print/_) -> 
            generate_source(printer, P2, Clauses)
     ; contains_goal(Clause,parse/_) ->
            generate_source(parser, P1, Clauses)
     ; generate_source(printer, P2, Rules1),
       generate_source(parser,  P1, Rules2),
       append(Rules1,Rules2, Clauses)
     ).


multiplex_expansion(A, B, Clauses) :-
     multiplex_modules(P1Mod, P2Mod),
     ( commit -> add_cut(B,A,B1) ; B = B1 ),
%    functor(A,Fun,Arity),
%    format(user_error,"Multiplex compiling...~a/~d~n",[Fun,Arity]),
     make_parse_clause(A, B1, P1Mod, Parser),
     make_print_clause(A, B1, P2Mod, Printer),
     expand_term(Parser,  P1),
     expand_term(Printer, P2),
     ( contains_goal(B,print/_) -> 
            generate_source(printer,P2,Clauses)
     ; contains_goal(B,parse/_) ->
            generate_source(parser, P1, Clauses)
     ; generate_source(printer, P2, Rules1),
       generate_source(parser,  P1, Rules2),
       append(Rules1,Rules2, Clauses)
     ).


/**
\predindex{multiplex\_modules/2}
\predindex{language/1}
\predindex{concat\_atom/2}
\predindex{normal\_expansion/2}
\predindex{contains\_goal/2}
\predindex{generate\_source/3}
\predindex{multiplex\_expansion/3}
\predindex{commit/0}
\predindex{add\_cut/3}
\predindex{make\_parse\_clause/4}
\predindex{make\_print\_clause/4}
We want pre-modularized DCG rules to be handled by
the normal mechanism. However, if we are reading
a language spec ({\tt language/1} is true), then
we add the module designation and ask the system
to process it.

If we are producing source-code (in the 
LEX or YACC mode) the clauses are generated
by this predicate.
**/
generate_source(Type, Module:Clause, []) :-
     call(Type),
     !,
     parser_stream(Module, Stream),
     format(Stream,'~q.~n',[Clause]).
generate_source(_, Clause, [Clause]).
/**
\predindex{generate\_source/3}
\predindex{parser\_stream/2}
When we open the files for the parser or printer source
code, we include a module definition with an empty
export list. This means that these predicates are all
hidden in this module and can only be called by
using the form {\tt Module:Goal}. This insures that
the user-defined predicates in a language specification
will never interfere with each other.
**/
parser_stream(Module, Stream) :-
     ( open_stream(Module,Stream) -> true
     ; concat_atom([Module,'.pl'],File),
       can_open_file(File, write, warn),
       open(File,write,Stream),
       format(Stream,':- module(~a,[]).~n',[Module]),
       name(Module,MChs),
       (append(LChs,"_parse",MChs) ->
         append([0''|LChs],"_plex.pl'",LexChs),
         name(Lexer, LexChs),
         name(Lang, LChs),
         format(Stream, ':- ensure_loaded(library(multi)).~n',[]),
         format(Stream, ':- dynamic   multi:language_loaded/1.~n',[]),
         format(Stream, ':- multifile multi:language_loaded/1.~n',[]),
         format(Stream, 'multi:language_loaded(~a).~n', [Lang]),
         format(Stream, ':- ensure_loaded(library(multi)).~n',[]),
         format(Stream, ':- ensure_loaded(~a).~n',[Lexer])
       ; true
       ),
       assert(open_stream(Module,Stream))
     ).
/**
\predindex{parser\_stream/2}
\predindex{open\_stream/2}
\predindex{concat\_atom/2}
\predindex{can\_open\_file/3}
Like the normal expansion of DCGs, this expansion,
adds two arguments for the data-structure (the 
{\it parse-tree}, in traditional terms).
By limiting access to this structure to a procedural
interface, we ensure that we can transform a grammar
into a generator. The {\tt update//2,3} access routines
add information to this structure during parsing and
extract the information during printing.

In addition to adding the data arguments to all user-defined
grammar rules, we will remove the special goals {\tt newline//0},
{\tt indent//0}, and {\tt undent//0} from the parsing version and
convert literals and tokens to character strings for the printing
version. 
The directives {\tt down/1} and {\tt up/0}
create a hierarchical structure. Finally, we must delay
the {\tt update//2,3} goals in the parser and advance
them in the printer so that the data is inserted or
extracted appropriately depending upon the intended
direction of the grammar.

This tool depends heavily on the module system. The
principle advantage of this is that we can preserve the
predicate names in the user's grammar while insuring that
different grammars do not interfere with each other.
This tool is designed to be a language ``mul<tiplexer''
with numerous parsers and generators active at one time.

There are three parts to a grammar rule and the order
of these three parts are different for parsers and printers.
A parse consists of grammar terminals and non-terminals,
followed by user code to process the data ({\tt user/1} goals),
which are then followed by the data-base updates.
In a printer, this order is reversed and we have data-base
access routines first, then the user code, followed by
the grammar terminals and non-terminals which actually
produce the text.

\begin{verbatim}
% PARSER
rule :-  keyword(K), [:], value(V), % Parse tokens
         user(norm_value(V,NV)),    % Compute NV from V
         update(K,keyword,NV).      % Add to data-base

% PRINTER
rule :-  update(K,keyword,NV),      % Extract from data-base
         user(norm_value(V,NV)),    % Compute V from NV
         keyword(K), [:], value(V). % Produce Text
\end{verbatim}

The {\tt move\_goals/4} predicate is the mechanism for
producing the different orderings. In the following code
to produce a parse clause, we first sweep the user code to
the end of the clause body, then sweep the data base updates
to the end of the clause so they will be immediately
after the {\tt user/1} goals. 
**/
make_parse_clause(Head, Body, Mod, Mod:(NewHead --> NewBody)) :-
   remove_goals(Body, [newline/0,indent/0,undent/0], Body1),
   move_goals(Body1, delay, [user], Body2),
   move_goals(Body2, delay, [update, updates,
                             seq_update, seq_updates,
                             examine,examines], Body3),
   add_data(Head,  parse, Mod, state(_,_,In),
                               state(_,_,Out), NewHead),
   add_data(Body3, parse, Mod, state(null, null,In),
                               state( _, null, Out), Body4),
   ( contains_goal(Body4,error/0) ->
      substitute_term(Body4,error,
                 multi:syntax_error(Mod,null,V),Body5),
      NewBody =  (multi:dcg_vars(V,V), Body5)
   ; contains_goal(Body4,error/1) ->
      substitute_term(Body4, error(X),
                 multi:syntax_error(Mod,X,V),Body5),
      NewBody =  (multi:dcg_vars(V,V), Body5)
   ;  NewBody = Body4
   ).
/**
\predindex{make\_parse\_clause/4}
\predindex{remove\_goals/3}
\predindex{move\_goals/4}
\predindex{add\_data/6}
\predindex{contains\_goal/2}
\predindex{substitute\_term/4}
To produce the print clause, we sweep the {\tt user/1}
goals to the front and then sweep the data-base
updates to be in front of them.
**/
make_print_clause(Head, Body, Mod, Mod:(NewHead --> NewBody)) :-
     move_goals(Body, advance, [user], Body1),
     move_goals(Body1, advance, [update, updates,
                                 seq_update, seq_updates,
                                 examine,examines], Body2),
     add_data(Head,  print, Mod, state(_,_,In),
                                 state(_,_,Out), NewHead),
     add_data(Body2, print, Mod, state(null,null,In),
                                 state(File, null,Out), Body3),
     tokens_to_strings(Body3, Body4),
     (File = file(Name,_,CVar) ->
        add_goal(Body4,
                     { multi:chars_to_file(Name, CVar) },
                 NewBody)
     ; NewBody = Body4
     ).

add_goal((A,B),Goal,(A,NB)) :- !, add_goal(B,Goal,NB).
add_goal(A,Goal,(A,Goal)).

/**
\predindex{make\_print\_clause/4}
\predindex{move\_goals/4}
\predindex{add\_data/6}
\predindex{tokens\_to\_strings/2}
\predindex{add\_goal/3}
\subsection{Data Management}

The greatest difficulty in building a universal translator
is to design the data structures and data management routines.
This representation must be suitable not only for
the languages under current consideration, but for
languages which have not been considered, even languages
which have not been designed yet.  The previous
empirical approach was to create a super-set language which
explicitly contained all of the types of the languages
being considered. Since expanding the translator was
a programming task, we expected to go into the program
and enrich the data structure whenever a new language
presented a different representation requirement.

Just as our choice of strategy of building deterministic
finite automata for the lexical analyzers does not 
place much restriction on the variety of languages 
which our system can handle, our choice of data structures
must be general enough to .
Our optimism even extends to the hope that, like
the lexical analyzer, the data management part of 
this tool will find wider use than hardware description
languages.

In the present implementation we assume that any language
the be reduced to a representation using only two object categories.
In our case the two concepts are  {\it id:type:value}
and hierarchical containment. Thus, a library contains
global attributes and cells, while cells contain
attributes describing its pins, delays, and logical functions.

The idea has a two-category representation has a
significant tradition ([Ajdukiewicz35], [Bar-Hillel50], [Curry50]).
A representation of a large fragment of natural language
has been based on two combinators (Noun and Sentence) [Curry58].
The conceptual graphs described by Sowa are another two-category
system based on objects and relations is another
candidate for an intermediate data-structure for translation.
Work at Virgina Tech (Cyre et. al.) is currently using Prolog
to construct conceptual graphs from informal
specifications and then to generate VHDL from these graphs [Cyre91].

The {\tt update//2} goals manage the primary
data structure representing the information being translated.
The bi-directionality of this data structure makes it
similar to the compiler dictionary described by [Warren80].

Our dictionary will contain primitive and hierarchical
data elements.  A primitive data element is a {\it name:value} or
{\it name:value-list} pair.
The hierarchical data-type is not visible to the user
and is managed by the special goals {\tt down//1} and {\tt up//0}.
The hierarchical capability allows us to manage
``scoping'' relationships along with the basic data elements,
without requiring direct manipulations of the Prolog structures.
Although the {\tt update//2} predicate will construct or
remove data from a tree structure for rapid (logarithmic time)
access, it is equivalent to:
\begin{verbatim}
update(Name, Value, DataIn, DataOut, X, X) :-
     ( var(X) ->
         remove(Name:Value, DataIn, DataOut)
     ;   DataOut = [Name:Value|DataIn]
     ).
\end{verbatim}

\subsection{Translating BNF to DCG}
We create a DCG grammar from the BNF grammar by adding
two arguments which represent the data-structure
entering and leaving a grammar rule. Once this processing
is completed, the {\tt term\_expansion/2} rules
pass the result to {\tt expand\_term/2} to convert
the DCG into Prolog (by adding the two arguments which
correspond to the token lists).

Thus, our arguments hide the parse-tree in exactly the same
way that DCG-syntax hides the parser's token lists.
Like DCG's we have our own special built-ins which must
be handled.  At this time we also consider whether
we are creating a parser or a printer and substitute
the appropriate built-ins. For example, the user wrote
{\tt expression(E)} in the language specification, but
we translate this to either {\tt parse\_expression} or
{\tt print\_expression}.

We tell {\tt add\_data/6} the direction (print or parse)
or the grammar, as well as the module name ({\tt Mod}).
We also pass a state variable which corresponds to the
the current file from which tokens are read ({\tt File}),
the hierarchy {\tt Level} and {\tt Var} 
(from {\tt up//0} and {\tt down//1} goals).
**/
% state(File, Level, Var)

add_data(Body, Dir, Mod, State, Final, NewBody) :-
    add(Body, Dir, Mod, State, Final, NewBody),
    !.

add_data(Body,_,_,state(F,L,Var),state(F,L,NewVar),New) :-
    Body =.. GList,
    append(GList, [Var, NewVar], NewGList),
    New =.. NewGList.

add((A,Rest), Dir, Mod, State, Final, (NewA,NewR)) :-
    add_data(A, Dir, Mod, State, NewState, NewA),
    add_data(Rest, Dir, Mod, NewState, Final, NewR).
/**
\predindex{add\_data/6}
\predindex{add/6}
Rules for every manner of built-in follow:
**/
add(parse(warning(E)), _, _,S,S,{write(E),nl}) :- !.
add(parse(error), _, _,S,S,error) :- !.
add(parse(error(E)), _, _,S,S,error(E)) :- !.
add(parse,           _, _,S,S,{true}).
add(print,           _, _,S,S,{true}).
add(error, _, _, S, S, error).
/**
\predindex{add/6}
**/
add(user(U), _, Mod, S, S, {Mod:U}).
add(operator(A,B,C), _, Mod, S, S, Mod:operator(A,B,C)).
add(primary(P), _, Mod, S, S, Mod:primary(P)).
add({Goal}, _, _, S, S, {Goal}).
add(!, _, _, S, S, !).
add([],     _, _, S, S,     []).
add([H|T],  _, _, S, S,  [H|T]).
add(optional(X), _, _, S, S, multi:optional(X)).
/**
**/
add(expression(E), print, Mod, S, S,
    multi:print_expression(E,Mod)) :- !.
add(expression(E), parse, Mod, S, S,
    multi:parse_expression(E,Mod)).
/**
\predindex{add/6}
**/
add(newline, _, _, S, S, multi:newline).
add(indent,  _, _, S, S, multi:indent).
add(undent,  _, _, S, S, multi:undent).
/**
**/
add(down(New), parse, _, state(F,Level,Curr),
         state(F,down(Curr,Level,New),[]), {true}) :- !.

add(down(New), print, _, state(F,Level,Curr),
         state(F, down(Curr, Level, _X), New),{true}).

/**
\predindex{add/6}
**/
add(up, parse, _, state(F,down(Prev, Level, Curr), Curr),
                        state(F,Level, Prev),{true}) :- !.

add(up, print, _, state(F, down(Prev, Level, _X), _Y),
                  state(F, Level, Prev), {true}) :- !.
/**
\predindex{add/6}
**/

add(up, _Dir, _Mod, _Si, _So, _G) :-
    bnf_error,
    format('''up'' without a matching ''down''!~n',[]),
    fail.
/**
\predindex{add/6}
\predindex{bnf\_error/0}
New ones.
**/
add(update(X), Dir, _, state(F,L,In), state(F,L,Out),
     {Update}) :- !,
     ( Dir == print ->
       Update = multi:print_update(X,In,Out)
     ; Update = multi:parse_update(X,In,Out)
     ).

add(updates(X), Dir, _, state(F,L,In), state(F,L,Out),
     {Update}) :- !,
     ( Dir == print ->
       Update = multi:print_updates(X,In,Out)
     ; Update = multi:parse_updates(X,In,Out)
     ).
/**
\predindex{add/6}
**/
add(seq_update(X), Dir, _, state(F,L,In), state(F,L,Out),
     {Update}) :- !,
     ( Dir == print ->
       Update = multi:print_seq_update(X,In,Out)
     ; Update = multi:parse_update(X,In,Out)
     ).

add(seq_updates(X), Dir, _, state(F,L,In), state(F,L,Out),
     {Update}) :- !,
     ( Dir == print ->
       Update = multi:print_seq_updates(X,In,Out)
     ; Update = multi:parse_updates(X,In,Out)
     ).
/**
\predindex{add/6}
**/
add(examine(X), Dir, _, state(F,L,In), state(F,L,Out),
     {Update}) :- !,
     ( Dir == print ->
       Update = multi:print_examine(X,In), In = Out
     ; Update = multi:parse_update(X,In,Out)
     ).

add(examines(X), Dir, _, state(F,L,In), state(F,L,Out),
     {Update}) :- !,
     ( Dir == print ->
       Update = multi:print_examines(X,In), In = Out
     ; Update = multi:parse_updates(X,In,Out)
     ).
/**
\predindex{add/6}
**/
add(file(F,L), Dir, Mod, State, NewState, Goals) :-
     source_or_sink(Dir, F, L, Mod, State, NewState, Goals).
/**
\predindex{add/6}
\predindex{source\_or\_sink/7}
**/
add(parse_string(A,B), Dir, Mod, state(F,L,Var),
                                 state(F,L,NVar), PS) :-
    (Dir == print ->
      PS = multi:parse_string_print(A,B,Mod,Var,NVar)
    ; PS = multi:parse_string_parse(A,B,Mod,Var,NVar)
    ).
/**
\predindex{add/6}
**/
source_or_sink(parse, Fname, Lex, Mod,
               state(_,  Level, Data),
               state(file(Fname,Lex,_), Level, Data),
               ( { multi:file_to_tokens(Fname,Lex,Mod,Tokens) },
                   multi:dcg_vars(_,Tokens) ) ).

/**
**/
source_or_sink(print, Fname, Lex, _Mod,
               state(InFile, L, D),
               state(file(Fname, Lex, NewVar),  L, D),
               Goals ) :-
     ( InFile = file(LastFile, _, Chars) ->
       Goals = ( { multi:chars_to_file(LastFile, Chars) },
                  multi:dcg_vars(_, NewVar) ) 
     ; Goals = multi:dcg_vars(_,NewVar)
     ).
/**
\predindex{source\_or\_sink/7}
**/
dcg_vars(In, Out, In, Out).
dcg_append(A,B,C) :- append(A,C,B).
/**
\predindex{dcg\_append/3}
\section{Advanced Features}

The next two sections describe MULTIPLEX facilities for
handling multi-level languages, tokenizing input files,
and the related facilities for general expression parsing.

\subsection{File I/O: The Tokenizer and Printer}
Calls to tokenizers are made from the {\tt file//2} goals
in the grammar. When the user specifies a file name and
a lexicon, the file is tokenized by that tokenizer before
being passed to the parser.  By allowing file specification
and tokenizers to be part of the grammar, a grammar spanning
many files and many different lexicons can be defined.
**/
file_to_tokens(File, Lex, Module, Tokens) :-
    filename(File, Filename),
    ( can_open_file(Filename, read, warn) -> true
    ; user:message_hook(file_existence_error(Filename),_,_)
    ),
    see(Filename),
    get_file(32,Chars),
    seen,
    Lexer =.. [ Lex, Chars, Tokens0 ],
    ( call(Module:Lexer) -> true
    ; user:message_hook('lexical error'(Filename),_,_)
    ),
    process_macros(Module, Tokens0, Tokens).
/**
\predindex{file\_to\_tokens/4}
\predindex{filename/2}
\predindex{can\_open\_file/3}
\predindex{message\_hook/3}
\predindex{get\_file/2}
\predindex{process\_macros/3}
Tokenizing the file includes optional macro-preprocessing,
for example, in ``C'' or Verilog. If the language specification
contains no ``macro'' definitions, {\tt process\_macros/3}
simply returns the token-list.
The corresponding predicate in the printer sends the
output to the specified file {\it after} the
textual form of the language has been produced by the grammar.
**/
chars_to_file(File, Chars) :-
    filename(File, Filename),
    ( can_open_file(Filename, write, warn) -> true
    ; user:message_hook(file_creation_error(Filename),_,_)
    ),
    open(Filename,write,Stream),
    write_list(Chars, 0, Stream),
    nl(Stream),  % Add a newline for good measure.
    close(Stream),
    !.
/**
\predindex{chars\_to\_file/2}
\predindex{filename/2}
\predindex{can\_open\_file/3}
\predindex{message\_hook/3}
\predindex{write\_list/3}
The {\tt filename/2} utility will make optional 
string substitutions in a file name.
An example of this string substitution is:
\begin{verbatim}
    ?- assert(current_name(foo)).
    yes.
    ?- filename('name.xyz', Filename).

    Filename = 'foo.xyz'

\end{verbatim}
**/
filename(File, Filename) :-
    name(File, FileChs),
    substitute(FileChs, NewChs),
    name(Filename, NewChs).
/**
\predindex{filename/2}
\predindex{substitute/2}
**/
substitute([],[]) :- !.

substitute(In, Out) :-
    append("name", RestIn, In),
    !,
    current_name(Name),
    name(Name, Chs),
    append(Chs, RestOut, Out),
    substitute(RestIn, RestOut).

substitute(In, Out) :-
    option(Option, Value),
    append(Option, RestIn, In),
    !,
    append(Value, RestOut, Out),
    substitute(RestIn, RestOut).

substitute([C|T],[C|NT]) :- substitute(T,NT).
/**
\predindex{substitute/2}
\predindex{current\_name/1}
\predindex{option/2}
**/
bnf_error :-
    seeing(File),
    current_input(Stream),
    stream_position(Stream,SPos),
    arg(2,SPos,Line),
    seen,
    put(7),put(7),
    format('Error on line ~d in File ~a~n',[Line,File]).
/**
\predindex{bnf\_error/0}
Our calls to {\tt update} must be at the end of grammar
rules in the parser and at the beginning of rules in
the printer. Therefore we must move these goals while
keeping the others in the proper order. We also must not
allow these goals pass over hierarchy goals
({\tt up//0} and {\tt down/1}).
**/
move_goals(In, Direction, Templates,  Out) :-
    move_goals(In, Direction, Templates, Out, T, T).

move_goal(advance, A, (A,Front), Front,    Delay, Delay).
move_goal(delay,   A,    Front, Front, (A,Delay), Delay).

opposite(advance, delay).
opposite(delay, advance).

move_goals((A,B), Direction, Preds, Front0, Delay0, Join) :-
    predicate_member(A, Preds),
    !,
    move_goal(Direction, A, Front0, Front, Delay0, Delay),
    move_goals(B, Direction, Preds, Front, Delay, Join).
/**
\predindex{move\_goals/4}
\predindex{move\_goals/6}
\predindex{predicate\_member/2}
**/
move_goals((A,B), Direction, Templates, Front0, Delay0, Join) :-
    !,
    opposite(Direction, Opposite),
    move_goal(Opposite, A, Front0, Front, Delay0, Delay),
    move_goals(B, Direction, Templates, Front, Delay, Join).
/**
\predindex{move\_goals/6}
**/
move_goals(Goal, Direction, Templates, Front0, Delay0, Join) :-
    predicate_member(Goal, Templates),
    !,
    move_goal(Direction, Goal, Front0, Join, Delay0, {true}).
/**
\predindex{move\_goals/6}
\predindex{predicate\_member/2}
**/
move_goals(Goal, Direction, _, Front0, Delay0, Join) :-
    opposite(Direction, Opposite),
    move_goal(Opposite, Goal, Front0, Join, Delay0, {true}).
/**
\predindex{move\_goals/6}
**/
tokens_to_strings(([C|Cs],B), (NewA, NewB)) :-
    !,
    tokens_to_strings([C|Cs], NewA),
    tokens_to_strings(B, NewB).
tokens_to_strings((A,B), (A, NewB)) :-
    tokens_to_strings(B, NewB).
/**
\predindex{tokens\_to\_strings/2}
When creating the pretty-printer, any terminals in lists must
be converted to character strings. However, if the terminal
is not instantiated, we must put in runtime calls to convert
the atom to a list of characters.
**/
tokens_to_strings([], {true}) :- !.

tokens_to_strings([T|Ts], ({multi:my_name(T,Tchs)},
                            multi:dcg_append(Tchs),Rest)) :-
   variable(T),
   !,
   tokens_to_strings(Ts, Rest).
/**
\predindex{tokens\_to\_strings/2}
\predindex{variable/1}
If the atom is instantiated at compile time, then we
simply insert a call to append it to the DCG list.
**/
tokens_to_strings([T|Ts], (multi:dcg_append(Tchs),Rest)) :-
   nonvar(T),
   functor(T,_,0),
   !,
   my_name(T,Tchs),
   tokens_to_strings(Ts, Rest).

tokens_to_strings(Goal, Goal).
/**
\predindex{tokens\_to\_strings/2}
\predindex{my\_name/2}
**/
variable(V) :-
     ( var(V) -> true
     ; functor(V,T,1),
       special_term(T)
     ).

special_term(integer).
special_term(float).
special_term(number).
special_term(quote).
/**
\predindex{variable/1}
\predindex{special\_term/1}
**/
remove_goals((A,B), Templates, Result) :-
   !,
   ( predicate_member(A, Templates) ->
      Result = Rest 
   ;  Result = (A, Rest)
   ),
   remove_goals(B, Templates, Rest).

remove_goals(Goal, Templates, Result) :-
   ( predicate_member(Goal, Templates) ->
      Result = ({true})
   ;  Result = Goal
   ).
/**
\predindex{remove\_goals/3}
\predindex{predicate\_member/2}
**/
predicate_member(Goal, Templates) :-
   functor(Goal, Functor, Arity),
   ( memberchk(Functor/Arity, Templates) -> true
   ; memberchk(Functor, Templates)
   ).
/**
\predindex{predicate\_member/2}
\predindex{memberchk/2}
Various other built-ins ({\tt subcell//1} is generated from
the hierarchy commands) are defined here.
**/
%       DataIn = [subcell(ID,Data)|DataOut]
%    ;  DataOut = [subcell(ID,Data)|DataIn].

subcell(ID, Data, DataIn, DataOut,Y,Y):-
    var(Y) ->
       remove(subcell(ID,Data),DataIn,DataOut)
    ;  DataOut = [subcell(ID,Data)|DataIn].

optional(Opt,In,Out) :-
    ( var(In) ->
           concat_atom(Opt,Atom), 
           my_name(Atom,Chars),
           append([32|Chars],Out,In)
         ; append(Opt,Out,In) ->
           true
         ; In = Out
    ).

newline([10|Xs],Xs).
indent([indent|Xs],Xs).
undent([undent|Xs],Xs).

/**
\predindex{subcell/6}
\predindex{remove/3}
\predindex{optional/3}
\predindex{concat\_atom/2}
\predindex{my\_name/2}
**/
write_list([],_,_).
write_list([C|Cs],Indent,Stream) :-
        process(C,Indent,Stream,NewIndent),
        write_list(Cs,NewIndent,Stream).
/**
\predindex{write\_list/3}
\predindex{process/4}
**/
process(10,Indent,Stream,Indent)        :- !,
        put(Stream,10), putn(Indent,32,Stream). 

process(indent,Indent,Stream,NewIndent) :- !,
        NewIndent is Indent + 4,
        process(10,NewIndent,Stream,_).

process(undent,Indent,Stream,NewIndent) :- !,
        NewIndent is Indent - 4,
        process(10,NewIndent,Stream,_).

process(float(F),Indent,Stream,Indent) :- !,
        format(Stream,'~f',[F]),
        put(Stream,32).

process(C,Indent,Stream,Indent) :- put(Stream,C).
/**
\predindex{process/4}
\predindex{putn/3}
**/
putn(N,C,Stream) :- 
     ( N < 1 -> true
     ; put(Stream,C),
       NN is N-1,
       putn(NN,C,Stream)
     ).
/**
\predindex{putn/3}
This version of name is unidirectional. It only produces character
strings from instantiated objects, hence we generate an error
string for the output file if someone tries to use it the other way round.

Due to a bug in {\tt library(charsio)} we must postpone 
processing the floating point numbers until we are ready
to write to a file (see {\tt process/4/4}).
**/
my_name('IsVaR',_) :- !,
    ( language(Out) -> true ; Out = unknown ),
    concat_atom([Out,'_print'],Module),
    user:message_hook('printing error'(Out,Module),_,_).

my_name(number(F),Chars)   :-
    !,
    (float(F) -> my_name(float(F),Chars)
    ;            my_name(integer(F),Chars)
    ).

my_name(number(N,Base,Pre,Post),Chars)   :-
    !,
    convert(N,Base,String),
    append(Pre,String,Front),
    append(Front,Post,Chars).

%my_name(float(F),[32|Chars])   :-
%   !,
%   with_output_to_chars(format('~f',[F]), Float),
%   remove_zeros(Float,Chars).

my_name(float(F),[32|FloatString])   :-
    !,
    (var(F) -> FloatString = "<<<ERROR>>> "
    ; FloatString = [float(F)]
    ).

my_name(quote(Q),[32,QuoteChar|Chars])   :-
      !,
      quote_type(Q,QuoteChar,String),
      append(String,[QuoteChar],Chars).

my_name(integer(*),[32|Chars]) :- !, my_name('IsVaR',Chars).
my_name(integer(I),[32|Chars]) :- !, number_chars(I,Chars).

my_name(name(N),[32|Chars])    :- !, name(N,Chars).
my_name(id(N),  [32|Chars])    :- !, name(N,Chars).
my_name(N,[32|Chars])          :- !, name(N,Chars).
my_name(N,Chars)               :- !, name(N,Chars).

quote_type(single(Cs),0'',Cs).
quote_type(double(Cs),0'",Cs).
/**
\predindex{my\_name/2}
\predindex{language/1}
\predindex{concat\_atom/2}
\predindex{message\_hook/3}
\predindex{convert/3}
\predindex{quote\_type/3}
**/
float_chars(*,Chars) :- var(Chars),!, my_name(*,Chars).
float_chars(Atom,[C1|Cs]) :-
      fix(C1, Cs, Result), !,
      number_chars(Atom,Result).
float_chars(Atom, Cs) :-
      number_chars(Atom, Cs).
/**
\predindex{float\_chars/2}
\predindex{my\_name/2}
\predindex{fix/3}
**/
fix(0'., Cs,       [0'0,0'.|Cs]).
fix(0'+, [0'.|Cs], [0'0,0'.|Cs]).
fix(0'-, [0'.|Cs], [0'-,0'0,0'.|Cs]).
/**
**/
scan_past(Pattern, In, Out) :-
   append(Pattern, Out, In) -> true
   ; In = [_|In1],
   scan_past(Pattern,In1,Out).

scan_string(Pattern, String, In, Out) :-
   ( append(Pattern, Out, In) -> String = []
   ; String = [S|String1],
     In = [S|In1],
     scan_string(Pattern, String1, In1, Out)
   ).
/**
\predindex{scan\_past/3}
\predindex{scan\_string/4}
\subsection{MULTIPLEX Options}
The MULTIPLEX recognizes the several options. These
options are handled by passing the command-line list (L)
to {\tt multiplex\_set\_options(+L,-R)}. This goal
will return any remaining items (R) from the command line.
Below we itemize these options and give the code
which handles it.
\begin{itemize}
\item{\bf -C}

Multiplex will force the parsing and printing routines to be
deterministic on self-recursive rules. This can make the
program more efficient but will fail to work on some
grammars. Use this option with caution.
**/
multiplex_set_options(['-C'|Opts],R) :- !,
    assert(multi:commit),
    multiplex_set_options(Opts,R).
/**
\predindex{multiplex\_set\_options/2}
\item{\bf -lexer \it language}

Produce a lexical analyzer for the specified language
and then terminate the program.
**/
multiplex_set_options(['-lexer'|Opts],R) :- !,
    assert(plex:lexer),
    multiplex_set_options(Opts,R).
/**
\predindex{multiplex\_set\_options/2}
\item{\bf -parser \it language}

Produce a parser for the specified language
and then terminate the program. All of the predicates
for this parser will be defined in the module
{\it language\tt \_parse}.
**/
multiplex_set_options(['-parser'|Opts],R) :- !,
    assert(multi:parser),
    multiplex_set_options(Opts,R).
/**
\predindex{multiplex\_set\_options/2}
\item{\bf -printer \it language}

Produce a printer for the specified language
and then terminate the program. All of the predicates
for this printer will be defined in the module
{\it language\tt \_print}.
**/
multiplex_set_options(['-printer'|Opts], R) :- !,
    assert(multi:printer),
    multiplex_set_options(Opts,R).
/**
\predindex{multiplex\_set\_options/2}
\item{\bf -all \it language}

Produce a lexical analyzer, parser and printer
for the specified language as described above.
**/
multiplex_set_options(['-all'|Opts],R) :- !,
    assert(plex:lexer),
    assert(multi:parser),
    assert(multi:printer),
    multiplex_set_options(Opts, R).
/**
\predindex{multiplex\_set\_options/2}
\item{\bf -S \it option value}

Replace occurrences of {\it option} with {\it value}
throughout the filename specifiers in the grammar rules.
**/
multiplex_set_options(['-S', Option, Value | T], NT) :- !,
    name(Option, OptChs),
    name(Value,  ValChs),
    assert(multi:option(OptChs, ValChs)),
    multiplex_set_options(T, NT).
/**
\predindex{multiplex\_set\_options/2}
\item{\bf -v}

Produce messages about the status of the translation.
**/
multiplex_set_options(['-v'|Os], NOs) :- !,
    assert(multi:verbose),
    assert(plex:verbose),
    multiplex_set_options(Os,NOs).
/**
\predindex{multiplex\_set\_options/2}
\end{itemize}

All other options  (except a plea for help)
are passed back to the caller.
**/
multiplex_set_options([Help|_],_) :-
   member(Help,['?','-?',help,'-help','Help','-Help','U','-U',u,'-u']),
   write('multiplex <Options> language | file.language'),nl,
   write('Options =  [-lexer] [-parser] [-printer | [-all]'),nl,
   write('           [-C(ommit optimization)]  [-v(erbose)]'),nl,
   write('           [-S(et) name value]'),nl,
   abort.

multiplex_set_options([Arg|T],[Arg|NT] ) :-
    !,
    multiplex_set_options(T, NT).

multiplex_set_options([],[]).
/**
\predindex{multiplex\_set\_options/2}
When using MULTIPLEX as a source-code generator, we must
explicitly close all streams before terminating.
**/
multiplex_quit :-
      open_stream(_,S),
      close(S),
      fail
    ; halt.
/**
\predindex{multiplex\_quit/0}
\predindex{open\_stream/2}
\subsection{Parsing Multi-level Languages}

Some languages achieve a superficial simplicity by embedding
complex constructions inside quoted strings.
We describe these as multi-level languages.
Two examples are the {\it Foresight} language from NuThena
and the synthesis library format of Synopsys.
In the case of {\it Foresight}, its LISP syntax is undisturbed by
the ADA-like {\it MINISPEC} language that lurks within.
Similarly, the Synopsys language defines the
boolean function of a cell with the following syntax:
\begin{verbatim}
      function: "a + b'c"
\end{verbatim}
The surface grammar of this statement is just:
\begin{verbatim}
      statement ::= [function, :, quote(double(S)),';'].
\end{verbatim}
With this rule, we can ``parse'' a Synopsys library
without dealing with the complexities of this boolean expression.
But at some point we will need to parse the expression
within the string {\tt S}.
Like the {\it MINISPEC} language in Foresight, there must be another
part of the tool which interprets this string.
Since many powerful languages are not multi-level
(VHDL, Verilog) we want MULTIPLEX to span these levels
transparently and derive the complete information of the
language. This poses several problems.

We cannot assume that the the same lexical rules
apply to the embedded language. Thus, we need to specify
which {\tt lexicon} to use. We
introduce {\tt parse\_string//2} which has the form:
\begin{verbatim}
         parse_string(Lexicon, Rule)
\end{verbatim}
By including this in a grammar rule, we can interrupt
the normal token stream, interpret the next token as
a user-defined {\tt String} type containing a character
string. We extract this character string, apply the
tokenizer identified by {\tt Lexicon} and parse
this token list with {\tt Rule}.  After
this non-terminal is processed, the appropriate variables
in {\tt Rule} (if any) will have the parsed structure and
the original token list of the parent language will once
again be in effect. To use this feature to analyze the
{\tt function:} statement in a Synopsys library, we say:
\begin{verbatim}
      statement ::= [function, :],
                    parse_string(syn, expression(F)),
                   [';'].
\end{verbatim}
As is typical with complex language processing in Prolog,
this is easier to accomplish than explain.
Like the other built-ins, we implement two versions of
{\tt parse\_string//2}, one for parsing and one for printing.
**/

get_string(quote(double(String)), String) :- !.
get_string(String,String).

parse_string_parse(Lex,Goal,Module,DataIn,DataOut,[St|Ts],Ts) :-
    get_string(St,String),
    append(String,[32],PaddedString),
    Lexer =.. [Lex, PaddedString, Tokens],
    ( Goal = expression(A) ->
            NewGoal = parse_expression(A,Module,Tokens,[]),
            DataIn = DataOut,
            Fun/Ar = expression/1
    ; Goal =.. [Fun|Args],
      length(Args,Ar),
      append(Args,[ DataIn, DataOut, Tokens, [] ],NewArgs),
      ExpandedGoal =.. [Fun|NewArgs],
      NewGoal = Module:ExpandedGoal
    ),
    ( call(Module:Lexer) -> true
    ; user:message_hook('lexical error'(lexicon(Lex,Fun/Ar)),_,_)
    ),
    ( call(NewGoal) -> true
    ; user:message_hook('parsing error'(Fun/Ar,Module),_,_)
    ).
/**
\predindex{get\_string/2}
\predindex{parse\_string\_parse/7}
\predindex{message\_hook/3}
**/
parse_string_print(_, Goal, Module, DataIn, DataOut, Out, Ts) :-
      ( Goal = expression(A) ->
          NewGoal = print_expression(A,Module,GenString,[]),
          DataIn = DataOut,
          Fun/Ar = expression/1
      ; Goal =.. [Fun|Args],
        length(Args,Ar),
        append(Args,[DataIn, DataOut, GenString,[]],NewArgs),
        ExpandedGoal =.. [Fun|NewArgs],
        NewGoal = Module:ExpandedGoal
      ),
      ( call(NewGoal) -> true
      ; user:message_hook('string printing error'(Fun/Ar,Module),_,_)
      ),
      process_embedded(GenString,String),
      append([0'"|String],[0'"|Ts],Out).
/**
\predindex{parse\_string\_print/7}
\predindex{message\_hook/3}
\predindex{process\_embedded/2}
For now we remove all
formatting commands and newlines out of internal strings.
If an embedded language has special escapes for including
newline characters, we should handle that here.
**/
process_embedded([],[]).
process_embedded([X|T],RT) :-
    ( X = indent -> NT = RT
    ; X = undent -> NT = RT
    ; X =:= 10   -> NT = RT
    ; RT = [X|NT]
    ),
    process_embedded(T,NT).
/**
\predindex{process\_embedded/2}
\subsection{{\it Commit}-- A Deterministic Optimization}
If the user specifies {\tt -C} on the multiplex
command line, a heuristic will be employed
to improve the performance of MULTIPLEX.
This optimization adds a cut (!) to any tail-recursive
clause at the point just before the recursion.
The simplicity of this optimization carries the penalty
that it will cause certain grammars to fail.
\begin{verbatim}
rule1(Ns,N) ::=  name_list(Ns), name(N), [';'].

name_list([N|Ns]) ::= name(N), name_list(Ns).
name_list([])     ::= [].
\end{verbatim}
This fragment of a grammar depends upon the ability of {\tt name\_list//1}
to backtrack, since its first solution reads too many names.
The automatic cut placement eliminates this
ability to backtrack. Two solutions to this problem are
to avoid the "-C" option or to re-write the grammar as:
\begin{verbatim}
rule1([],    X) ::= name(X), [';'].
rule1([N|Ns],X) ::= name(N), rule1(Ns,X).
\end{verbatim}
Which is simpler and more elegant anyway. Unfortunately, 
this rule gains no advantage from the {\tt -C} optimization
but at least it will not fail.

The following predicate adds the penultimate cut
if the last goal names the same predicate as the head.
The logic to see if this option is turned on is
in the {\tt term\_expansion/2} rule.
**/
add_cut((!,B),_,(!, B)) :- !.  % already has a cut
add_cut((A,B),G,(A,B1)) :- !, add_cut(B,G,B1).
add_cut(   G, H, Body)  :-
     functor(G,F,A),
     ( functor(H,F,A) -> Body = (!,G)
     ;                   Body = G
     ).
/**
\predindex{add\_cut/3}
**/
substitute_term(Var,_,_,Var) :- var(Var), !.

substitute_term([],_,_,[]) :- !.

substitute_term([H|T],Pattern, Var, [NH|NT]) :-
      !,
      substitute_term(H,Pattern,Var, NH),
      substitute_term(T,Pattern,Var, NT).

substitute_term(Pattern,Pattern,Var,Var) :- !.

substitute_term(Term, Pattern, Var, NewTerm) :-
    Term =.. [Functor|List],
    substitute_term(List, Pattern, Var, NewList),
    NewTerm =.. [Functor|NewList].
/**
\predindex{substitute\_term/4}
\section{A Note on Program Organization}
There are several issues which strongly affected the
organization of this program and will thus affect any
other programs built upon these modules.
The interactions of modules within a multiplex translator
and the desire to allow language specifications to
be compiled separately (for quick loading) are the
most important influences on the complexity of MULTIPLEX.

The end-user program is called {\tt trans}. We compile the
program by compiling the file {\tt trans.pl} which defines
the main routine and includes the error handling code
{\tt errors.pl} for the program. All of this code appears
in the module {\tt user}. The {\tt trans} program includes
the {\tt multiplex} module (file).  This module includes
most of the parser/generator code as well as many utility
predicates for the lexical analyzer and parsers (in {\tt runtime.pl}).

The {\tt multiplex.pl} file includes the module {\tt plex.pl} to
create a lexical analyzer. It is important to realize that the
user's code which is manipulated and generated by the plex program
is not automatically included in the module {\tt plex}. It
is included (by default) into whichever module calls {\tt plex/3}
which in this case is the term-expansion predicates of {\tt multiplex}.
Thus, unless otherwise designated, the lexical analyzer predicates
appear in the {\tt user} the module, and thus the user-included 
calls to built-in predicates (on the left-hand side of LEX rules)
are made in the {\tt user} module.  For this reason, multiplex
provides aliases of the built-ins in module {\tt user}.

There are in fact, two situations which arise. In one situation
the lexical analyzer will be looking for the built-ins in the
module {\tt multiplex} and will find them there. In the second,
it will be looking for the built-ins in the module user -- and
will depend upon these aliases (by alias, we simply mean a trivial
predicate of the form {\tt user:built\_in(X) :- multiplex:built\_in(X).}).

When multiplex ``consults'' a language specification file,
the term-expansion rules in module {\tt multiplex} compile
the generated code into module {\tt multiplex}. However, 
if we wish to acellerate the loading of language specifications
we can pre-compile a specification file with the command:
\vskip 0.2cm
\centerline{\tt qpc -c -i multiplex.pl lang.spec}
\vskip 0.2cm
Although this command includes necessary term-expansion
rules by  pre-loading {\tt multiplex.pl}, the predicates
are compiled into module {\tt user} by default. When
the file {\tt lang.spec.qof} is loaded, the un-designated
predicates will appear in module {\tt user}, the module
in which they were compiled by {\tt qpc}, and not
in the module {\tt multiplex} which issued the {\load\_files/1}
command. This behavior which could easily be non-portable.

\section{Future Work}

We have created a language-independent translation
system which can construct and execute specialized
translators with no need for separate compilation steps.
This program is used by DASIX/Intergraph
to translate VLSI synthesis model libraries.
Although this program was developed as a translator,
it could be used as a front- and back-end
of any tool to compile, analyze, and verify
formal languages.

MULTIPLEX can also function as three separate tools to
create tokenizers, parsers, or pretty-printers, from
a language specification.  These tools,
here embedded in the MULTIPLEX program, improve
on their predecessors (LEX and YACC) in the following ways:
\begin{itemize}
\item{}
Automatic generation of printers (text generators) complements
the support for tokenizers and parsers.
\item{}
Any number of tokenizer/parser/generators can co-exist
because of the unique naming of synthesized predicates.
\item{}
Intermediate compilation steps are eliminated -- the program proceeds
immediately from reading the language spec to parsing the language.
\end{itemize}

The design of this program depended heavily on
Prolog features such as user-defined term-expansion,
grammar rule notation, and the ability to modify
Prolog syntax through operator definitions.
The program could not exist in its present
form without Prolog's ability to manipulate code as data,
and to do this immediately prior to execution.

The simplicity of a two-category system creates a
problem for the data-management routines. 
When the primary functor of a type class is one of two
values, then the indexing (pattern matching) capabilities
are only using a single bit out of the 32-bits inherent
in every object.

The {\tt update//2} routine can be improved by computing
a hash value (index) from the functors of complex types.

Preliminary performance measurements showed that
parsing with potentially non-deterministic grammar rules
required more memory that the equivalent deterministic parser.
\begin{verbatim}
   % ls -l vgt350.fpdl
   -rw-rw-r--  1 pbr        255338 Nov  6 00:00 vgt350.fpdl
   % time trans vgt350.fpdl x.fpdl (without cuts)
   630.5u 24.7s 11:32 94% 0+6180k  8+53io  1077pf+0w 
   639.3u 26.5s 12:07 91% 0+6292k  3+45io  1477pf+0w
   637.8u 24.3s 11:24 96% 0+6480k 54+46io   637pf+0w
   639.3u 27.7s 12:52 86% 0+5868k 37+44io 1788pf+0w
\end{verbatim}
A routine to add cuts before tail recursive calls
in grammar rules (hypothesized to be a safe
restriction on this sort of grammar) gave us
a 10% improvement in overall performance which
was largely due to a 20% reduction in the dynamic 
memory requirement (1.3MB out of 6.3MB).

Currently, multiplex requires the following resources
to transform a 250KB FPDL file on a 16M SparcStation 1+.
\begin{verbatim}
   % ls -l vgt350.fpdl
   -rw-rw-r--  1 pbr        255338 Nov  6 00:00 vgt350.fpdl
   % time transcut vgt350.fpdl x.fpdl (with cuts)
   515.0u  9.5s  9:05 96% 0+5620k  5+44io   4pf+0w
   489.6u  6.4s  8:23 98% 0+5580k  5+44io   0pf+0w (specialized)
   503.0u  7.3s  8:35 99% 0+5636k 12+42io  10pf+0w
   (spec w/predictive parser)
   497.6u  9.6s  8:32 99% 0+5608k  4+54io   0pf+0w  old
   505.5u  7.5s  8:45 97% 0+5472k  7+47io  70pf+0w (new add)
   536.5u  6.8s  9:05 99% 0+5620k 35+46io  33pf+0w (with error)

  383.7u 170.6s 11:14 82% 0+9292k 245+26io 3273pf+0w (new version 2/92)
\end{verbatim}
Particularly note the large number of page faults. 
This is partially due to the fact that the workstation
configuration made only about 9MB of memory available.

This project fulfilled the requirement that the
language descriptions be ``externalized'',
but much can be done to both simplify and extend
the expressiveness of these descriptions.
At present, the user must avoid left-recursive grammar
rules and though
rules may backtrack, they must be deterministic
in the sense that they only have one solution if the
{\tt -C} optimization is to be used.

In the future we may find that complex languages
require us to re-order clauses as well as goals
to achieve complete bi-directionality. This would
require a technique more general than term-expansion
in order to operate on predicates rather than clauses.

The use of Prolog syntax for the external specification
language simplifies the design of this tool and
encourages us to consider the general use of Prolog as
an embedded (user oriented) language in CAD systems.
The program transformation techniques described here can
provide non-expert programmers with special purpose
languages which are easier to use than Prolog.
Although these languages are less powerful than Prolog,
they can greatly widen the audience for logic programming.
\code{
**/

contains_univ((A,B),N) :-
     !,
     ( functor(A,'=..',N) -> true
     ; contains_univ(B,N)
     ).

contains_univ((_ :- B),N) :- 
     !,
     contains_univ(B,N).

contains_univ(A,N) :-
     functor(A,'=..',N).

/**
\predindex{contains\_univ/2}
\section{Macro Substitutions}
This algorithm maintains macro definitions
in the Prolog database.
**/

process_macros(Module, In, Out) :-
      predicate_property(Module:macro_start(_,_,_),_),
      abolish(Module:macro/3),
      !,
      process_macros1(In, Module, Out).
process_macros(_, Ts, Ts).

/**
\predindex{process\_macros/3}
\predindex{process\_macros1/3}
}\endcode
\code{
**/

process_macros1([], _, []) :- !.
process_macros1(Tokens0, Mod, Tokens) :-
      Mod:macro_start(Type, Tokens0, [Name|Tokens1]),
      !,
      macro_def(Type, Name, Mod, Tokens1, Tokens2),
      process_macros1(Tokens2, Mod, Tokens).

process_macros1(Tokens0, Mod, Tokens) :-
      Mod:macro_use(Macro, Tokens0, Tokens1),
      Mod:macro(Macro, Tokens1, Tokens2),
      !,
      process_macros1(Tokens2, Mod, Tokens).

process_macros1([H|Tokens0], Mod, [H|Tokens]) :-
      process_macros1(Tokens0, Mod, Tokens).
      
/**
\predindex{process\_macros1/3}
\predindex{macro\_start/3}
\predindex{macro\_def/5}
\predindex{macro\_use/3}
\predindex{macro/3}
\section{Atomic and Functional Macros}
Macro definitions are of two types, simple token
replacement (atomic) and parameterized macros (function).
The {\tt macro\_def/4} predicate handles these different
kinds of macros by creating different {\tt macro/3}
assertions.  When a {\tt macro\_use} occurs, we will call
{\tt macro/3} to perform the appropriate transformation.
**/

macro_def(atomic, Name, Mod, Tokens0, Tokens) :-
      macro_body(Mod, Body, Tokens0, Tokens),
      redefine_macro(Name, Mod),
      assert(Mod:(macro(Name, In, Out):-
                   append(Body, In, Out))),
      \+ cycle(Body, Mod, Name).

macro_def(function, Name, Mod, Tokens0, Tokens) :-
      macro_parameters(Params, Tokens0, Tokens1),
      macro_body(Mod, Body, Tokens1, Tokens),
      substitute_parameters(Params, Vs, Body, NewBody),
      redefine_macro(Name, Mod),
      assert(Mod:(macro(Name, T0, T) :-
                   multi:macro_parameters(Vs, T0, T1),
                   append(NewBody, T1, T) ) ),
      \+ cycle(Body, Mod, Name).
/**
\predindex{macro\_def/5}
\predindex{macro\_body/4}
\predindex{redefine\_macro/2}
\predindex{cycle/3}
\predindex{macro\_parameters/3}
\predindex{substitute\_parameters/4}
**/
macro_vars([])     --> [ ')' ], !.
macro_vars([V|Vs]) --> [ ',', V ], macro_vars(Vs).

redefine_macro(Name, Mod) :-
    (retract(Mod:macro(Name, _, _)) ->
       user:message_hook(macro_redefined(Name),_,_)
     ; true
    ).
/**
\predindex{macro\_vars//1}
\predindex{redefine\_macro/2}
\predindex{message\_hook/3}
**/
macro_body(Mod, [])   --> Mod:macro_end, !.
macro_body(Mod,[H|T]) --> [H], macro_body(Mod,T).
/**
\predindex{macro\_body//2}
\predindex{macro\_end//0}
**/
macro_parameters([P|Ps]) --> 
          ['(',P], !,
          macro_parameters1(Ps).

macro_parameters1([])     --> [')'], !.
macro_parameters1([P|Ps]) --> 
          [',', P ],
          macro_parameters1(Ps).
/**
\predindex{macro\_parameters//1}
\predindex{macro\_parameters1//1}
**/
substitute_parameters([], [])         --> [].
substitute_parameters([P|Ps], [V|Vs]) -->
       substitute_parameter(P, V),
       substitute_parameters(Ps,Vs).


substitute_parameter(_, _, [], []) :- !.

substitute_parameter(P, V, [P0|Tokens0], [T|Tokens]) :-
       (P == P0 -> V = T ;  P0 = T ),
       substitute_parameter(P, V, Tokens0, Tokens).

/**
\predindex{substitute\_parameters//2}
\predindex{substitute\_parameter//2}
\predindex{substitute\_parameter/4}
\section{Checking for Cyclic Macros}

The C-preprocessor finds recursive macros by falling
into them. After about 100 iterations, it notices
this and quits trying to re-write the macro. 
Rather than taking this rather drastic approach, we
can efficiently check for cyclic macro definitions
if we are 1) checking a specific macro and 2) know that
all other macros are acyclic. Thus, we must make
this check after adding each macro to ensure a 
consistent set of macro definitions.
}\endcode
\code{
**/

cycle(Body, Mod, Name) :-
      reference(Body, Mod, Name),
      user:message_hook(recursive_macro(Name),_,_).

reference((_,Body), Mod, Name) :-
     reference(Body, Mod, Name).
reference(append(List,_,_), Mod, Child) :-
     reference(List, Mod, Child).
reference([Goal|_], _, Child) :-
     nonvar(Goal),
     functor(Goal, Child, _),
     !.
reference([Goal|_], Mod, Child) :-
     nonvar(Goal),
     functor(Goal, Name, _),
     clause(Mod:macro(Name,_,_), Body),
     reference(Body, Mod, Child),
     !.
reference([_|T], Mod, Child) :-
     reference(T, Mod, Child).

/**
\predindex{cycle/3}
\predindex{reference/3}
\predindex{message\_hook/3}
The {\tt macro_type\2} predicate decides
whether the macro has parameters (e.g.  {\tt foo( ) })
or is atomic (e.g. {\tt foo (... )}
**/

macro_type([H|T], Type) :-
      ( ctypes:is_alnum(H) ->
         macro_type1(T,Type)
      ;  macro_type(T, Type)
      ).

macro_type1([0'(|_], func_define) :- !.
macro_type1([H|T], Type) :-
      ctypes:is_alnum(H),
      !,
      macro_type1(T, Type).
macro_type1(_, atom_define).
/**
\predindex{macro\_type/2}
\predindex{is\_alnum/1}
\predindex{macro\_type1/2}
% \section{The Multiplex Runtime Environment}
\subsection{User Accessible Routines in MULTIPLEX}
The primary functions of MULTIPLEX can be seen
by examining three user accessible routines.
{\tt Multiplex\_input/2} takes the name of a file
and returns the data parsed from that file.
It expects the suffix of the file name to tell
it which language the file contains. It calls
{\tt multiplex\_build/1} with this suffix and
assumes that a tokenizer and parser have been
constructed if {\tt multiplex\_build/1} succeeds.
It then constructs a call to the top-level
rule of the grammar and calls it (it assumes the name of
this rule is equal to the suffix).
**/
multiplex_input(In, Data) :-
    multiplex_suffix(In, Root, Suffix),
    make_current(Root),
    ( multiplex_build(Suffix) -> true
    ; write('Could not build parser for language'(Suffix)),
      nl,fail
    ),
    Parser =.. [ Suffix, [], Parsed, _, [] ],
    concat_atom([Suffix,'_parse'], Module),
    ( verbose -> format("Reading ~a~n",[In]) ; true),
    ( call(Module:Parser) -> true
    ; user:message_hook('parsing error'(In,Module),_,_)
    ),
    multiplex_reverse_data(Parsed, Data).
/**
\predindex{multiplex\_input/2}
\predindex{multiplex\_suffix/3}
\predindex{make\_current/1}
\predindex{multiplex\_build/1}
\predindex{concat\_atom/2}
\predindex{verbose/0}
\predindex{message\_hook/3}
\predindex{multiplex\_reverse\_data/2}
**/
multiplex_output(Out, Data) :-
    multiplex_suffix(Out,Root,Suffix),
    make_current(Root),
    multiplex_build(Suffix),
    Printer =.. [ Suffix, Data, _Leftovers, _, [] ],
    concat_atom([Suffix, '_print'], Module),
    assert(language(Suffix)),
    ( verbose -> format("Writing ~a~n",[Out]) ; true),
    ( call(Module:Printer) -> true
    ; user:message_hook('printing error'(Out,Module),_,_)
    ),
    retract(language(Suffix)).
/**
\predindex{multiplex\_output/2}
\predindex{multiplex\_suffix/3}
\predindex{make\_current/1}
\predindex{multiplex\_build/1}
\predindex{concat\_atom/2}
\predindex{verbose/0}
\predindex{message\_hook/3}
{\tt Multiplex\_build/1} can be called at any time. If
the language in question has already been loaded, it
simply succeeds.  After setting a few flags and making
sure that the language specification file is accessible,
the language modules are constructed by
{\tt consult/1}.
}\endcode
\code{
**/
multiplex_build(Language) :-      % No Need to Load Language
    language_loaded(Language),
    !.

multiplex_build(Language) :-
    concat_atom([Language,'.spec'], Spec),
    can_open_file(Spec, read, warn),
    concat_atom([Spec,'.qof'],  Object),
    multiplex_build(Spec, Object, Language),
    ( retract(language(Language)) -> true ; true ),
    asserta(language_loaded(Language)).

multiplex_build(Spec, Object, _) :-         % Spec file is most recent
    can_open_file(Object, read, fail),
    file_property(Object,modify_time, OTime),
    file_property(Spec,  modify_time, STime),
    OTime @> STime,
    !,
    (verbose -> user:message_hook(loading(Object),_,_);true),
    load_files(Object).

multiplex_build(Spec, Object, _) :- % Object doesn't exist, but
    no_style_check(discontiguous),

    fail,  % THIS FEATURE HAS BEEN TURNED OFF

    fast_compile(Spec),             % it can be compiled by qpc
    !,
    (verbose -> user:message_hook(loading(Object),_,_);true),
    load_files(Object).

multiplex_build(Spec, Object, Language) :-
    (verbose -> user:message_hook(consulting(Spec),_,_) ; true),
    consult(Spec),
    ( (parser ; printer) -> true
    ; concat_atom([Language,'_parse'],ParseModule),
      concat_atom([Language,'_print'],PrintModule),
      ( save_modules([ParseModule,PrintModule],Object) -> true
      ; write('Failure to save'(ParseModule,PrintModule)),nl
      ),
      user:message_hook(saving(Object),_,_)
    ).

fast_compile(Spec) :- 
   prolog_flag(runtime_directory, Bin),
   concat_atom([Bin,'/qpc'], QPC),
   file_exists(QPC,1),            % Executable QPC
   concat_atom([QPC,' -c -i multi.pl ',Spec], CMD),
   write(CMD),nl,
   (verbose -> user:message_hook(compiling(Spec),_,_);true),
   unix(shell(CMD)).
/**
\predindex{multiplex\_build/1}
\predindex{language\_loaded/1}
\predindex{concat\_atom/2}
\predindex{can\_open\_file/3}
\predindex{multiplex\_build/3}
\predindex{file\_property/3}
\predindex{verbose/0}
\predindex{message\_hook/3}
\predindex{fast\_compile/1}
\predindex{parser/0}
\predindex{printer/0}
\predindex{file\_exists/2}
**/
object_out_of_date(Language, Spec, Object, Compile) :-
    concat_atom([Language,'.spec'], Spec),
    can_open_file(Spec, read, warn),
    file_property(Spec,  modify_time, STime),
    concat_atom([Spec,'.qof'],  Object),
    ( can_open_file(Object, read, fail),
      file_property(Object,modify_time, OTime),
      OTime @> STime ->
        Compile = no
    ;   Compile = yes
    ).
/**
\predindex{object\_out\_of\_date/4}
\predindex{concat\_atom/2}
\predindex{can\_open\_file/3}
\predindex{file\_property/3}
}\endcode
\code{
\subsection{Facilities for Expression Parsing}

In two of the languages we've considered, embedded languages
are used for the representation of boolean expressions.
Synopsys, for example represents the function
for a library cell:
\begin{verbatim}
          function: "(a + b')' ((c b)+a')(d+e')"
\end{verbatim}
This string contains a complex expression syntax
with a postfix unary operator (') and an implicit binary
operator which has stronger
precedence than the {\tt +} operator.
If the user can specify
the precedence, fixity, and appearance of all operators,
together with a rule for primary expressions,
MULTIPLEX will correctly parse all
expressions. This feature addresses two important 
difficulties in language-based tool development.

Documentation for many industrial languages
frequently gives little or no guidance for
implementing expression parsers. When such documents
include BNF rules for expression syntax, the
definitions are often left-recursive, and thus
unsuitable for a recursive descent parser.
For these reasons we do not want the language-spec writer
to spend time defining complex rules for
expressions. Since all expression parsers have similar
features, different types of expressions can be distinguished
by specifying a few facts about the language. For 
a relatively simple language (FPDL) the specification is:
\begin{verbatim}
operator(200,xfy,  or) ::= ['|'].
operator(300,xfy, and) ::= ['&'].
operator(100, fx, not) ::= ['~'].

primary(Expr)           ::=  ['('], expression(E), [')'].
primary(V)              ::=  value(V).
\end{verbatim}

\centerline{\bf Figure 3: Example operator and primary expressions definition}

\vskip 0.2cm
With these definitions ({\tt value/1} is
defined elsewhere in the user specification),
a call to the MULTIPLEX built-in grammar rule {\tt expression(E)}
will parse expressions like:
\begin{verbatim}
       A & 1 & ~B | ((A | B & C) & ~(A|B)& | 0)
\end{verbatim}
To define a language with function expressions
in which the function arguments can also be expressions,
we need only write:
\begin{verbatim}
primary(function(F,As)) ::= name(F), ['('], arguments(As).

arguments([])     ::= [')'].
arguments([A|As]) ::= expression(A), more_args(As).

more_args([])     ::= [')'].
more_args([A|As]) ::= [','], expression(A), more_args(As).
\end{verbatim}
The problems presented by the Synopsys language
(the implicit AND operator, postfix and prefix NOT operator)
are easily handled by this system.
The following definitions will work:
\begin{verbatim}
operator(200,xfy,  or) ::= [+].
operator(300,xfy, and) ::= [*].
operator(300, xy, and) ::= [].
operator(100, xf, not) ::= [''''].
\end{verbatim}
\centerline{\bf Figure 4: Operators for Logic Formula with Implicit AND}
\vskip 0.2cm
Note that the Synopsys language also contains an
explicit AND operator (*) and so we include
this rule before the "empty AND" rule. Internally,
these are represented identically as {\tt expr(Left,and,Right)}
and when the textual form of the language is being produced,
the first rule is used causing the (*) operator
to appear explicitly in all generated formula.

The operator specifications in MULTIPLEX are similar
to those in Prolog, even in the choice of the argument
order for {\tt (Precedence, Fixity, Operator)}. The most
important difference is the addition of the {\tt xy} fixity 
specification defining the implied, or invisible, operator.
Clearly, a language can only have one implied operator,
such as AND in logical formula and multiplication in arithmetic.
**/
parse_expression(Expr,M) --> 
    parse_expression(1200, Expr, M).

parse_expression(_,M,List,_) --> 
    { user:message_hook('expression error'(List,M),_,_) }.
/**
\predindex{parse\_expression//2}
\predindex{parse\_expression//3}
\predindex{parse\_expression//4}
\predindex{message\_hook/3}
}\endcode
\code{
**/
parse_expression(Precedence, Result, M) -->
    prefixop(Op, Oprec, Aprec, M),
    !,
    parse_expression(Aprec, Expr, M),
    { NewExpr =.. [ Op, Expr ] },
    exprtl(Oprec, NewExpr, Precedence, Result, M).
/**
\predindex{parse\_expression//3}
\predindex{prefixop//4}
\predindex{exprtl//5}
**/
parse_expression(Precedence, Result, M) -->
    M:primary(Expr),
    !,
    exprtl0(Expr, Precedence, Result, M).

/**
\predindex{parse\_expression//3}
\predindex{primary//1}
\predindex{exprtl0//4}
The following is a stripped down version of Warren and O'Keefe's
expression parser for the public domain version of {\tt read.pl}.
I have added code for a new kind of infix operator which
I call ``implicit'' operators. These are invisible operators
like multiplication in standard mathematical notation.
For example, in the expressions:
\begin{verbatim}
     (a + b)(c + d)     'a'b + c'd + c(d)
\end{verbatim}
**/

exprtl0(Term, Precedence, Answer, M, S1, S) :-
    ambigop(F, Precedence, L1, O1, R1, L2, O2, M, S1, S2),
    !,
    (exprtl(0,Term,Precedence,Answer,M,
            [infixop(F,L1,O1,R1)|S2],S)
    ;exprtl(0,Term,Precedence,Answer,M,
            [postfixop(F,L2,O2) |S2],S)
    ).
/**
\predindex{exprtl0/6}
\predindex{ambigop/10}
\predindex{exprtl/7}
**/
exprtl0(Term, Precedence, Answer, M, S1, S) :-
    postfixop(F, L2, O2, M, S1, S2),
    !,
    exprtl(0, Term, Precedence, Answer, M,
           [postfixop(F,L2,O2) |S2], S).
/**
\predindex{exprtl0/6}
\predindex{postfixop/6}
\predindex{exprtl/7}
**/
exprtl0(Term, Precedence, Answer, M, S1, S) :-
    infixop(F, L1, O1, R1, M, S1, S2),
    !,
    exprtl(0, Term, Precedence, Answer, M,
               [infixop(F,L1,O1,R1)|S2], S).
/**
\predindex{exprtl0/6}
\predindex{infixop/7}
\predindex{exprtl/7}
**/

exprtl0(Term, _, Term, _) --> [].

exprtl0(Term, Precedence, Answer, M, S1, S) :-
    ( S1 == [] -> fail ; true ),
    imp_infixop(F, L1, O1, R1, M, S1, S2),
    exprtl(0, Term, Precedence, Answer, M,
              [infixop(F,L1,O1,R1)|S2], S).

exprtl0(Term, _, Term, _) --> [].
/**
\predindex{exprtl0//4}
\predindex{exprtl0/6}
\predindex{imp\_infixop/7}
\predindex{exprtl/7}
**/
exprtl( _, Term, _, Term, _, [], []). %DONE

exprtl( C, Term, Precedence, Answer, M,[T|Ts],Rest) :-
    exprtl(T, C, Term, Precedence, Answer, M, Ts, Rest).
/**
\predindex{exprtl/7}
\predindex{exprtl/8}
}\endcode
\code{
**/
exprtl(infixop(F,L,O,R), C, Term,Precedence,Answer,M,S1,S) :-
    Precedence >= O, C =< L,
    !,
    parse_expression(R, Other, M, S1, S2),
    NewExpr =.. [ F, Term, Other ],
    exprtl(O, NewExpr, Precedence,Answer,M,S2,S).
/**
\predindex{exprtl/8}
\predindex{parse\_expression/5}
\predindex{exprtl/7}
**/
exprtl(postfixop(F,L,O),C,Term,Precedence,Answer,M,S1,S) :-
    Precedence >= O, C =< L,
    !,
    peepop(M, S1, S2),
    NewExpr =.. [ F, Term ],
    exprtl(O, NewExpr, Precedence, Answer, M, S2, S).

exprtl(Token, _, Term, _, Term, _, Tokens, [Token|Tokens]).
/**
\predindex{exprtl/8}
\predindex{peepop/3}
\predindex{exprtl/7}
For each prefix or postfix operator we generate two
precedence values to reflect the associativity.
**/
prefixop(F, O, Q, M) --> M:operator(O, fx, F), { Q is O-1}.
prefixop(F, O, Q, M) --> M:operator(O, fy, F), { Q is O }.
/**
\predindex{prefixop//4}
\predindex{operator//3}
**/
postfixop(F, P, O, M) --> M:operator(O, xf, F), {P is O-1}.
postfixop(F, P, O, M) --> M:operator(O, yf, F), {P is O}.
/**
\predindex{postfixop//4}
\predindex{operator//3}
Infix operators require three (possibly different) precedence
values to represent the combination of precedence value and
associativity.
**/
infixop(F,P,O,Q,M) --> M:operator(O, xfy, F),{!,P is O-1,Q is O}.
infixop(F,P,O,Q,M) --> M:operator(O, xfx, F),{!,P is O-1,Q is P}.
infixop(F,P,O,Q,M) --> M:operator(O, yfx, F),{  Q is O-1,P is O}.
/**
\predindex{infixop//5}
\predindex{operator//3}
The new ``implied'' operators are just the same as the actual
infix operators, but they use the new fixity identifiers
({\tt xy},{\tt xx}, and {\tt yx}).
**/
imp_infixop(F,P,O,Q,M) --> M:operator(O,xy,F),{!,P is O-1,Q is O}.
imp_infixop(F,P,O,Q,M) --> M:operator(O,xx,F),{!,P is O-1,Q is P}.
imp_infixop(F,P,O,Q,M) --> M:operator(O,yx,F),{  Q is O-1,P is O}.
/**
\predindex{imp\_infixop//5}
\predindex{operator//3}
In some cases we know which kind of infix operator we will
accept. On other occasions, we will accept any infix operator.
**/
any_infixop(F,P,O,Q,M) --> infixop(F,P,O,Q,M).
any_infixop(F,P,O,Q,M) --> imp_infixop(F,P,O,Q,M).
/**
\predindex{any\_infixop//5}
\predindex{infixop//5}
\predindex{imp\_infixop//5}
An ambiguous operator is one defined as both a postfix and an infix
operator (because of our procedure we never have a
prefix/infix ambiguity). This call to {\tt infixop/7} is
an example of where we only want to consider the printable
infix operators.
**/
ambigop(F, Precedence, L1, O1, R1, L2, O2,M) -->
    postfixop(F, L2, O2, M),
    { O2 =< Precedence,
          infixop(F, L1, O1, R1, M,  _, _),
      O1 =< Precedence
    }.
/**
\predindex{ambigop//8}
\predindex{postfixop//4}
\predindex{infixop/7}
The look-ahead predicate which identifies operators is
more complex than in Warren and O'Keefe because
it most be non-deterministic.
The first two cases, in which postfix or explicit
infix operators are identified is standard.
**/
peepop(M, [F|S1], [postfixop(Op,L,P)|S1]) :-
       postfixop(Op, L, P, M, [F|x], x), !.

peepop(M, [F|S1], [infixop(Op,L,P,R)|S1]) :-
       infixop(Op, L, P, R, M, [F|x],x), !.
/**
\predindex{peepop/3}
\predindex{postfixop/6}
\predindex{infixop/7}
The next two clauses interpret the empty list first
as an empty list...
**/
peepop(_, S, S).
/**
And failing that, as a list containing an implicit operator.
**/
peepop(M, [H|S1], [infixop(Op,L,P,R),H|S1]) :-
       imp_infixop(Op, L, P, R, M, _, _).
/**
\predindex{peepop/3}
\predindex{imp\_infixop/7}
\subsection{Printing Expressions}.
Printing expressions is fairly trivial, but we cheat a bit.
Currently, we assume that the primary expression syntax uses parenthesis
for grouping.  To do this correctly we should extract whatever
grouping characters appear in the user's {\tt primary/1} definitions.
}\endcode
\code{
**/
print_expression(null,_) --> !, [].

print_expression(id(P),_) --> !,
          { my_name(P,Tchs) },
          dcg_append(Tchs).

print_expression(number(P),_) --> !,
          { my_name(number(P),Tchs) },
          dcg_append(Tchs).

print_expression(Expr, Module) -->
       { functor(Expr,Op,2), !,
         arg(1,Expr,P1),
         arg(2,Expr,P2) },
      "(", print_expression(P1,Module),
           Module:operator(_,_,Op),
           print_expression(P2,Module), ")".
/**
\predindex{print\_expression//2}
\predindex{my\_name/2}
\predindex{dcg\_append//1}
\predindex{operator//3}
If a unary operator (like NOT) has both prefix and postfix
versions, we should take the first rule in the user's 
grammar as the preferred method of printing. 
Some people might actually PREFER postfix, which would
not be an obvious choice. The embedded
if-then-else is our way of choosing only the first
definition in the context of a DCG.
**/
print_expression(Expr, Module) -->
      "(", { functor(Expr,Op,1),
             ( Module:operator(_,U,Op,_,_) -> (U==fx;U==fy)),
             !,
             arg(1,Expr,P)
           },
           Module:operator(_,U,Op),
           !,
           print_expression(P,Module),  ")".
   

print_expression(Expr, Module) -->
      "(", { functor(Expr, Op, 1),
             !,
             arg(1,Expr, P)
           },
           print_expression(P,Module),
           Module:operator(_,xf,Op),  ")".
/**
\predindex{print\_expression//2}
\predindex{operator/5}
\predindex{operator//3}
**/
print_expression(P,_) -->
          { atomic(P), my_name(P,Tchs) },
          dcg_append(Tchs).
/**
\predindex{print\_expression//2}
\predindex{my\_name/2}
\predindex{dcg\_append//1}
One of the subtle differences between {\tt print\_updates/6} and
{\tt parse\_updates/6} is that we must insure that there is
at least one of these items to print.
It is too easy to generate empty lists of things to print.
**/

/**
The ``plural'' calls \{updates/1, examines/1, and seq_updates/1\}
require that the first argument of the term will be a list.
If there is only one matching object, a list of
one element will be returned.

That is, we only make one assumption about the
data structure defined by the user, that the
first argument will be a list. Interestingly,
the types of objects making up the list is not
restricted in any way.
**/

parse_updates(Term, In, Out) :-
    arg(1,Term,List),
    parse_updates(List, Term, In, Out).

parse_updates([], _, D, D) :- !.
parse_updates([ID|IDs], Term, D0, D) :-
    functor(Term, F, A),
    functor(H,    F, A),
    arg(1, H, ID),
    match_args(2, A, Term, H),
    parse_updates(IDs, Term, [H|D0], D).

parse_update(H, T, [H|T]).
/**
\predindex{parse\_updates/3}
\predindex{parse\_updates/4}
\predindex{match\_args/4}
**/
print_updates(Term, In, Out) :-
    arg(1, Term, [ID|IDs]), % must be at least one!
    print_updates([ID|IDs], Term, In, Out).

print_updates([H|T], Term, D0, D) :-
    print_named_update(H, Term, D0, D1),
    !,
    print_updates(T, Term, D1, D).
print_updates([], _, D, D).


print_named_update(ID, Term, [H|D0], D) :-
    ( arg(1, H, ID),
      functor(Term, F, A),
      functor(H,    F, A),
      match_args(2, A, Term, H) -> D0 = D
    ; D = [H|D1],
      print_named_update(ID, Term, D0, D1)
    ).

print_update(H, [H2|In], Out) :-
    (H = H2 -> In = Out
    ; Out = [H2|Rest],
      print_update(H, In, Rest)
    ).

match_args(From, To, Term1, Term2) :-
    ( From > To -> true
    ; arg(From, Term1, Arg),
      arg(From, Term2, Arg),
      Next is From + 1,
      match_args(Next, To, Term1, Term2)
    ).
    
print_seq_update(H, [H|T], T).

print_seq_updates(Term, D0, D) :-
     arg(1, Term, [ID|IDs]),   % Must have at least one!
     print_seq_updates([ID|IDs], Term, D0, D).

print_seq_updates([ID|T], Term, [H|D0], D) :-
    arg(1, H, ID),
    functor(Term, F, A),
    functor(H,    F, A),
    match_args(2, A, Term, H),
    !,
    print_seq_updates(T, Term, D0, D).
print_seq_updates([], _, D, D).

/**
\predindex{print\_updates/3}
\predindex{print\_updates/4}
\predindex{print\_named\_update/4}
\predindex{match\_args/4}
\predindex{print\_update/3}
\predindex{print\_seq\_updates/3}
\predindex{print\_seq\_updates/4}
This version of print_examine(s) only needs one argument
for the parse tree since it doesn't remove anything.
**/
print_examines(Term, D) :-
    arg(1, Term, [ID|IDs]),  % at least one!
    print_examines([ID|IDs], Term, D).

print_examines([ID|T], Term, D) :-
    print_named_examine(ID, Term, D),
    !,
    print_examines(T, Term, D).
print_examines([], _, _).

print_named_examine(ID, Term, [H|D]) :-
    ( arg(1, H, ID),
      functor(Term, F, A),
      functor(H,    F, A),
      match_args(2, A, Term, H) -> true
    ; print_named_examine(ID, Term, D)
    ).

print_examine(H, [H2|T]) :-
    (H = H2 -> true
    ; print_examine(H, T)
    ).

/**
\predindex{print\_examines/2}
\predindex{print\_examines/3}
\predindex{print\_named\_examine/3}
\predindex{match\_args/4}
\predindex{print\_examine/2}
}\endcode
\code{
**/
contains_goal(Goal,F/A)  :- contains_goal(Goal,F,A).

contains_goal((X,Y),F,A) :-
     !,
     ( functor(X,F,A) -> true
     ; contains_goal(Y,F,A)
     ).
contains_goal(X,F,A) :- functor(X,F,A).

syntax_error(Module, E, Front, After, _) :-
    two_lists(Front, After, 10, Left, Right),
    user:message_hook('syntax error'(_,Module,Left,Right,E),_,_).
/**
\predindex{contains\_goal/2}
\predindex{contains\_goal/3}
\predindex{syntax\_error/5}
\predindex{two\_lists/5}
\predindex{message\_hook/3}
**/

two_lists( List, List, N, [], ListB) :-
    !,
    second_list(N, List, ListB).
two_lists([H|T], List, N, [H|NT], ListB) :-
    two_lists(T, List, N, NT, ListB).
    
second_list(0, _, [])   :- !.
second_list(N, [H|T], [H|NT]) :-
     NN is N - 1,
     second_list(NN,T,NT).
/**
\predindex{two\_lists/5}
\predindex{second\_list/3}
\subsection{Utilites}
The rest of this section defines simple utility predicates
used by various parts of the program.
**/
write_clauses([],_).
write_clauses([C|Cs],Stream) :-
    format(Stream,'~q.~n',[C]),
    write_clauses(Cs,Stream).
/**
\predindex{write\_clauses/2}
**/
remove_zeros([C|Cs], [C|Rs]) :-
   (C =:= 0'. -> Cs = [C2|C2s],
                 Rs = [C2|R2s],
                 rtz(C2s, R2s)
   ; remove_zeros(Cs, Rs)
   ).
/**
\predindex{remove\_zeros/2}
\predindex{rtz/2}
**/
rtz(Float, Chars) :-
   ( append(Front, "0", Float) ->
     rtz(Front, Chars)
   ; Float = Chars
   ).
/**
\predindex{rtz/2}
**/
remove(H,[H|T],    T) :- !.
remove(A,[H|T],[H|R]) :-
      remove(A,T,R).
/**
\predindex{remove/3}
**/
multiplex_suffix(In, Root, Suffix) :-
     name(In,InChs),
     append(RootChs,[0'.|SufChs],InChs),
     name(Root,RootChs),
     name(Suffix,SufChs).
/**
\predindex{multiplex\_suffix/3}
**/
add_suffix(Name, Suffix, Root, Total) :-
    name(Name,NChs),
    ( append(RootChs, Suffix, NChs) ->
        name(Root, RootChs),
        Total = Name

    ;   Root = Name,
        append(NChs, Suffix, TChs),
        name(Total, TChs)
    ).
/**
\predindex{add\_suffix/4}
The conversion routines for different bases is
callable from the user defined grammar.
**/
convert(N, Base, Cs) :-
   var(N),
   !,
   convert(Cs, 0, Base, N).

convert(N, Base, Cs) :-
   int_to_string(N, Base, [], Cs).

int_to_string(N,Base,In,Out) :-
   (N =:= 0 -> In = Out
   ; Bulk is N div Base,
     CharNum is N mod Base,
     digit(Char, CharNum),
     int_to_string(Bulk, Base, [Char|In], Out)
   ).

convert([], N, _, N).
convert([C|Cs], In, Mul, Out) :-
      digit(C,Val),
      Next is In * Mul + Val,
      convert(Cs, Next, Mul, Out).
/**
\predindex{convert/3}
\predindex{convert/4}
\predindex{int\_to\_string/4}
\predindex{digit/2}
}\endcode
\code{
**/
digit(0'0,0).   digit(0'5,5).
digit(0'1,1).   digit(0'6,6).
digit(0'2,2).   digit(0'7,7).
digit(0'3,3).   digit(0'8,8).
digit(0'4,4).   digit(0'9,9).
/**
**/
digit(0'a,10).  digit(0'A,10).
digit(0'b,11).  digit(0'B,11).
digit(0'c,12).  digit(0'C,12).
digit(0'd,13).  digit(0'D,13).
digit(0'e,14).  digit(0'E,14).
digit(0'f,15).  digit(0'F,15).
/**
**/
get_file(-1,  [32]) :- !.
get_file(C, [C|Cs]) :- get0(NC), get_file(NC,Cs).
/**
\predindex{get\_file/2}
**/
multiplex_reverse_data([],[]).
multiplex_reverse_data([H|T], Rev) :-
    ( multiplex_reverse_data(H, RH) -> true ; RH = H ),
    rev_data(T, [RH], Rev).
multiplex_reverse_data(subcell(N,T,S),subcell(N,T,RS)) :-
    multiplex_reverse_data(S,RS).
multiplex_reverse_data(if(C,T,E),if(C,RT,RE)) :-
    rev_data(T,[],RT),
    rev_data(E,[],RE).
multiplex_reverse_data(case(C,Cs),case(C,RCs)) :-
    multiplex_reverse_data(Cs, RCs).
/**
\predindex{multiplex\_reverse\_data/2}
\predindex{rev\_data/3}
**/
rev_data([],    T,  T) :- !.
rev_data([H|T],In,Out) :-
    ( multiplex_reverse_data(H,RH) -> 
      rev_data(T,[RH|In],Out)
    ; rev_data(T,[H|In],Out)
    ).
/**
\predindex{rev\_data/3}
\predindex{multiplex\_reverse\_data/2}
**/
make_current(Name) :-
    (retract(multi:current_name(_))->true;true),
    assert(multi:current_name(Name)).

:- dynamic   user:term_expansion/2.
:- multifile user:term_expansion/2.

user:(term_expansion((A::=B), Clauses) :-
      multi:multiplex_expansion(A, B, Clauses)
     ).
/**
\predindex{make\_current/1}
The fourth line in this term expansion rule executes
the optimization described later in the section
``{\it Commit} -- A Deterministic Optimization''.

Normal Prolog clauses inside a specification file
must be duplicated in each of the modules. If
a clause is pre-modularized, we let the system
handle it.
**/
user:(term_expansion(_:_,_) :- !,fail).
user:(term_expansion(end_of_file, _) :- !, fail).
user:(term_expansion(Clause, Compile) :-
      multi:normal_expansion(Clause, Compile)
     ).
/**
}\endcode
**/
