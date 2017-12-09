:- module(plex,[ plex/3 ]).

:- use_module(library(sets)).
:- use_module(library(lists),[keys_and_values/3,last/2,delete/3,nth1/3]).
:- use_module(library(ordsets)).
:- use_module(library(basics),[member/2, memberchk/2]).
:- use_module(library(ctr)).
:- use_module(library(ctypes), [is_ascii/1,is_char/1]).
:- use_module(library(strings), [gensym/2, concat_atom/2]).
:- op(1200, xfx, lexicon).
:- op(800,  xfx, if).

:- dynamic user:term_expansion/2, lexer/0, multi:language/1.
:- multifile user:term_expansion/2, multi:language/1.

user:(term_expansion(lexicon(Lang,Rules), Compile) :-
      (multi:language(MLang) -> true
      ; assert(multi:language(Lang)),
        MLang = Lang
      ),
      ( plex:plex(Lang, Rules, Lexer) -> true
      ; user:message_hook('lexicon_error'(Lang), _, _)
      ),
      Clauses = [(:- no_style_check(discontiguous))|Lexer],
      ( plex:lexer -> 
              strings:concat_atom([Lang,'_plex.pl'], Out),
              open(Out, write, Stream),
              plex:write_clauses(
               [(:- ensure_loaded(plex))|Clauses],Stream),
              close(Stream),
              Compile = []
      ; strings:concat_atom([MLang,'_parse'], Module), 
        Compile = Module:Clauses
      )
     ).
/**
\long\def\code#1{}
\def\endcode{}
\chapter{PLEX: Generating Lexical Analyzers}

\section{Introduction}
PLEX is a system for creating lexical analyzers in Prolog.
It it most simply defined as a Prolog counterpart of the
popular UNIX lexical analyzer generator (LEX) described in
[Lesk75], [Johnson78], and [Aho85].  It uses a standard algorithm
for constructing Deterministic Finite Automata (DFA) from
patterns of regular-expressions.

PLEX improves upon LEX by providing a recursive model
in which lexers can call other lexers, it also allows any
number of lexical analyzers to co-exist without interference,
and can be used to construct lexical analyzers from
textual descriptions at run-time.

A lexical analyzer for a language is built around a 
finite automaton.  A finite automaton  recognizes
whether a given input string belongs to the language or not.
Such a recognizer is constructed by compiling the
regular expression specification into a finite automaton and
then simulating the input string on it.
The finite automaton that recognizes an input string
can be deterministic (DFA) or
non-deterministic (NFA). A NFA permits  more than
one transition  out of a state on the same input symbol.
It also permits $\epsilon$-transitions, {\it i.e.}  transitions
on the empty string.

There are many strategies for building NFAs and DFAs from
regular expression specifications, and they provide a range of
time-space tradeoffs. The usual procedure is to
first  construct an NFA from
a regular expression. If a DFA is needed then the 
{\it subset construction} [Ullman85] method is used to
convert the NFA  to a DFA.

There are tradeoffs to using DFAs and NFAs.
An NFA is compact but has prohibitive
time requirements when recognizing long input strings.  For a regular
expression $r$, and input string $x$,
it can construct
a transition table that requires $O(\mid r \mid)$ space, but takes
time $O((\mid r \mid \times \mid x \mid)$.
A DFA is fast - $O(\mid x \mid)$ time, but 
can require exponential space in the worst case, $O(2^{\mid r \mid})$.

 We are interested in deterministic finite automatons, 
and the program described in this report implements a slightly
modified version of an algorithm
[Ullman85] to  construct such a DFA. The motivation to
use this algorithm is that it  constructs a DFA from
a regular expression without constructing an intermediate NFA. The approach
is outlined in the Ullman book.  Briefly, it first
constructs a syntax tree for an augmented regular expression
$(r)${\tt accept}. The tag {\tt accept} is used to
mark accepting states of the regular expression, $r$.
The interior nodes of this tree
correspond to primitive regular expression operators
{\it union, concatenation,} and {\it Kleene closure}.
The leaves of the tree are symbols in the
alphabet or $\epsilon$ (the empty string). The symbol
$\epsilon$ is denoted by the marker {\tt empty}.
Each non-$\epsilon$
leaf is labeled with a unique position number. 
The DFA is constructed from the augmented regular expression
$(r)${\tt accept} in the following manner. First the
tree is  traversed to compute the functions
{\it  firstpos(n)},{\it lastpos(n)}, and {\it nullable(n)} on the
nodes of the tree.  These functions correspond
to  attributes of nodes in the parse tree.
The first two are sets of positions that
can match the first and last symbol of a string generated
by the subexpression rooted at node $n$.
The function {\it nullable(n)} is true if a subexpression
rooted at the node $n$ can generate a language that includes
$\epsilon$. Using these functions
the function {\it followpos(p)} is computed for each position in the set of
position numbers. If {\it p} is a position, then {\it followpos(p)}
is the set of positions {\it q} such that there is some input string
{\it ...xy...} such that {\it p} corresponds to this occurence of {\it x}
and {\it q} corresponds to this occurence of {\it y}.
This set of follow positions is
then used to compute states of the DFA. 
The algorithm to construct the DFA is shown below,
\begin{verbatim}
Dstates is the list of states in the DFA.

Initially, the only unmarked state in Dstates is firstpos(root),
where root is the root of the syntax tree for (r)accept;

while there is an unmarked state T in DStates do
   begin
      mark T;
      for each input symbol a do 
      begin
       let U be the set of positions that are in followpos(p)
         for some position p in T,
        such that the symbol at position p is a;
       if U is not empty and is not in DStates then
         add U as an unmarked state to DStates;
       Dtran[T,a] := U
      end
   end
\end{verbatim}

The state-transition clauses that are generated for
the DFA will have the form:
\vskip 0.2cm
\centerline{\tt symbol(+Char, +Chars, +Tail, +Head, +PrevToken, -Token, -Rest)}
\vskip 0.2cm
where {\tt symbol} is the language name,
{\tt Char} is the current character being considered,
{\tt  Chars} are the rest of the input characters, {\tt Tail} and
{\tt Head} are the respective parts of the character list
being recognized. This is also termed as the {\it lexeme}.
{\tt Token} is the returned token,
and {\tt Rest} represents the input characters remaining  after
the token has been recognized. {\tt PrevToken}
is actually a term of three arguments:
\vskip 0.2cm
\centerline{\tt prev(Cs,PToken,Action)}
\vskip 0.2cm
This term is used to cache
the last input position at which the DFA entered
an accepting state. {\tt PToken} is the token encountered there, 
{\tt Action} is the goal, and {\tt Cs} is the input character list remaining
after {\tt PToken} has been recognized. Only a single term memory is needed
because when the DFA fails to find a  next state for the 
current state-input symbol pair, it needs to backtrack to only the most recent 
accepting state.

Our modification to the above algorithm is 
only in the building of the regular expression syntax tree and its
attribute  computing phase. For space-efficiency reasons, and in order
to control the size of the tree  we have added new node types
that denote some of the non-primtive {\tt lex} operators, such as
character classes, complemented classes, repeated occurences etc.

\subsection{Definitions}
The {\bf multiplex} document describes the multiplex specification
along with  the lexicon and rule definitions. We augment them with
the following definitions in this document:
\begin{enumerate}
\item
A {\bf lexical rule}  consists of a
{\bf plex} regular expression  followed by
a result and an optional action. The result is typically the token returned
by the lexical analyzer.

\item
A {\bf plex} regular expression consists of
symbols in the alphabet ({\it i.e.} text symbols),
and  one or more primitive or non-primitive operators.

\item
A {\bf primitive operator} is one of the following three:
{\it union}, {\it Kleene closure}, or
{\it concatenation}. The {\it union} is shown by the $\mid$,
and the {\it closure} by $*$. Concatenation does not have
a symbol. It is indicated by juxtaposing the two symbols(or strings).

\item
A {\bf non-primitive operator} is a set of characters
that define complex strings in the language using a flexible
and concise notation. All regular expressions
written using the non-primitive operators can be
rewritten as expressions using the primitive operators.
The non-primitive operators have the same meaning
as in {\tt lex}. The ones currently supported are: 
{\tt [],-, ?, +, ., ,} \verb@^@,{\tt \$, \{ \}}.
\end{enumerate}

\subsection{Tutorial}
 This section is a brief tutorial describing how PLEX
can be used.  This program can be used as a stand-alone program
or as a  module in another program, to generate Prolog
tokenizers.  

When used as a program, it will read a
specification file and produce the source code for a
Prolog tokenizer. The specification file should have an
extension of {\tt .spec}. Shown below
is  a sample specification:
\begin{verbatim}
% xyz.spec  -- PLEX specification for language XYZ 

xyz lexicon
"[0-9]+"                       is integer(I)
                               if (text(T),number_chars(I,T));
"[0-9]+\.[0-9]+([eE]-?[0-9]+)? is float(F)
                               if (text(T),number_chars(F,T));
"[A-Za-z][A-Za-z_0-9@!\.]*"    is id(N) 
                               if (text(T),name(N,T));
"""[^""]"""                    is quote(T) if text(T);
"."                            is C
                               if (text(T),name(C,T)).
\end{verbatim}

Using Quintus Prolog,
a stand-alone lexical analyzer can be generated
with the {\tt multiplex} utility.
\begin{verbatim}
% multiplex -lexer xyz.spec   % CREATE TOKENIZER
% prolog
?- [xyz_plex].                % LOAD TOKENIZER xyz_plex.pl

?- xyz("Hello, there 3.14159",Tokens). % CALL TOKENIZER

Tokens = [id(Hello),',',id(there),float(3.14159)]
\end{verbatim}

When used as a module in another program, PLEX can be used
to build the tokenizer at {\bf compile} time from within
the program source. This makes it no longer necessary to write
separate specification files.  In the example
below, the file {\tt sample\_program.pl}, which is the source
program, uses plex as a module. Since the 
 specification file, {\tt xyz.spec} is 
 a Prolog predicate with user-defined operators ({\tt is, lexicon, if}),
it now  becomes part of the source.  During compile time, the
{\tt term\_expansion/2} predicate  defined in the program is
used to place a hook into PLEX. This invokes {\tt plex/3} which
generates the tokenizer {\tt xyz\_plex} from the specification.
The predicate {\tt tokenize\_chars/2} accepts a list of characters
and generates a list of tokens by invoking the tokenizer predicate
{\tt xyz/2}.
\begin{figure}[htb]
\begin{verbatim}
:- load_files(library(plex), when([both]).

xyz lexicon

"[0-9]+"                       is integer(I)
                               if (text(T),number_chars(I,T));
"[0-9]+\.[0-9]+([eE]-?[0-9]+)? is float(F)
                               if (text(T),number_chars(F,T));
"[A-Za-z][A-Za-z_0-9@!\.]*"    is id(N)
                               if (text(T), name(N,T));
"""[^""]"""                    is quote(T)
                               if text(T);
"."                            is C
                               if (text(T),name(C,T)).

% tokenize_chars(+Characters, -Tokens).
tokenize_chars(Characters, Tokens) :-
       xyz(Characters, Tokens).
\end{verbatim}
\caption{Building a Lexical Analyzer at Compile-time}
\end{figure}
Another advantage of PLEX is that it 
permits you to create lexical analyzers "on the fly",
as part of a user program. The UNIX {\tt lex} does not provide that
flexibility. In the program below, compiling the spec file
(via execution of predicate {\tt tokenize\_chars/3})
will create the tokenizer. Then the top-level predicate is called to
execute it.
\begin{figure}[htb]
\begin{verbatim}
:- use_module(library(plex)).

% tokenize_chars(+Language, +Characters, -Tokens) 

tokenize_chars(Language, Characters, Tokens) :-
       concat_atom([Language,'.spec'],SpecFile),
       consult(SpecFile),
       Lexer =.. [Language, Characters, Tokens],
       call(Lexer).
\end{verbatim}
\caption{Building a Lexical Analyzer at Run-time}
\end{figure}
The appendix describes a very simple PLEX specification file and
shows the lexical analyzer code generated for the DFA built from the
PLEX specification file.
**/
/**
\section{The Program}
 {\tt plex/3} is the top-level predicate that creates the 
lexical analyzer  clauses 
from the lexical rules in the spec file. These rules
are regular expressions written with operators to appear readable.
\vskip 0.2cm
\centerline{\tt  plex(+Lang,+LexRules,-Lexer) }
\vskip 0.2cm
{\tt Lang} specifies the language name, 
{\tt LexRules} is a disjunction of Prolog goals that specify the lexical rules
 in the spec file, and
{\tt Lexer} is a list of clauses that make up the DFA of the lexical analyzer.

After the  parse tree for the regular expression is
constructed, it is annotated with attributes at the various nodes.
These are the {\it firstpos, lastpos}, and {\it followpos}
attributes. The first two  correspond to the symbols that can appear at the
 first  and last position at each node.
The last attribute refers to a set of positions that can follow a
given position. The global counter {\tt pos/1} is
used for this purpose.  
Along with this annotation task, the following are retrieved: \\
{\tt Start}{\tt \_State} of the DFA,
the {\tt FollowposSet}, which is used to construct the
DFA, and {\tt Sym\_Pos\_List}, which is a list of symbol position 
pairs. Next, the {\tt construct\_dfa/5} predicate is called
to create the state transition table. This is then
used by  {\tt generate\_clauses/3} to generate the set
of clauses ({\tt Lexer}) for the lexical analyzer.
**/
:- op(800,  xfx, if).
:- op(1200, xfx, lexicon).
:- op(900,   fx, a).
:- op(1200, xfx, ':=').

:- dynamic pos/1, verbose/0.

plex(Lang, LexRules, Lexer):-
    vformat(user_error, "Creating Reg Expn Parse Tree...~n",[]),
    assert(pos(1)),  % Initialize counter to
    ctr_set(0, 1),    %  mark position numbers for symbols
    statistics(runtime,[T0|_]),
    parse_plex_rules(LexRules, ParseTree),
    vformat(user_error,"Creating Follow_Pos Table....~n~n",[]), 
    annotate_parsetree(ParseTree, Sym_PosList_Pairs,
                        Start_State, FollowPosSet),
    statistics(runtime,[T1|_]),
    T2 is T1-  T0,
    vformat(user_error,"~3d sec to parse and generate Follow pos~n",[T2]),
    vformat(user_error,"------Follow_Pos Table-----~n",[]),
    vformat(user_error,"Constructing DFA....~n",[]),
    construct_dfa(Lang, Start_State, Sym_PosList_Pairs,
                  FollowPosSet, StateTable),
    vformat(user_error,"------DFA QuadTriples-~n",[]),
    vformat(user_error,"Generating clauses...~n",[]),
    statistics(runtime,[T3|_]),
    lex_top_level(Lang, TopLevel),
    generate_clauses(StateTable, Lang, Clauses, []),
    statistics(runtime,[T4|_]),
    T5 is T4-  T3,
    vformat(user_error,"Took ~3d sec to generate clauses~n",[T5]),
   append(TopLevel, Clauses, Lexer).

/**
\predindex{plex/3}
\predindex{vformat/3}
\predindex{ctr\_set/2}
\predindex{parse\_plex\_rules/2}
\predindex{annotate\_parsetree/4}
\predindex{construct\_dfa/5}
\predindex{lex\_top\_level/2}
\predindex{generate\_clauses/4}
\subsection{Regular Expressions and Syntax Tree Construction}

The regular expression parser  constructs an augmented syntax tree
from an input PLEX specification.  The construction process
is outlined below. 

 The first set of  predicates {\tt parse\_plex\_rules/4,3}
parse the user-defined operator definitions in the
lexical rules. They then call the DCG rule {\tt parse\_reg\_expn//1}
to parse the regular expressions.

\subsubsection{Syntax Tree Data Structure}
An entire PLEX specification is  collected at the top
level as a {\it list} of terms. Each term is a parsed PLEX rule
denoting an individual regular expression separated by a `;' in
the PLEX specification. This  is of the form
{\tt rule(Rexp,Result, Goal)}.
{\tt Rexp} is the  syntax tree  representing the parsed regular expression,
for each PLEX  rule.
Each such  regular expression syntax tree in a rule is
augmented by appending a right-end marker {\tt accept} to it, thus distinctly
denoting an accepting state for the regular expression.

The data structure of the parsed regular expression syntax tree,{\tt Rexp} is a
node structure of the form:
\vskip 0.2cm
\centerline{\tt node(Tag,Nullable,First,Last) }
\vskip 0.2cm
where {\tt Nullable, First} and {\tt Last} are attributes
at each node describing the values of the functions:
{\it nullable(), firstpos()} and {\it lastpos()}.
{\tt Tag} is one of the following:
\begin{description}
\item[leaf(C,Pos):] Denotes a leaf node where {\tt C} could refer
to a character, a character class, or the {\tt accept} symbol.
{\tt Pos} is the position number. Character classes are assigned
a single position number. Since they correspond to the union of
the characters in the class, they have the  same {\it firstpos()}
and {\it lastpos()} values. For every character class with $c$ characters
in a regular expression specification this yields a savings of $c-1$ nodes
in the regular expression syntax tree and $c-1$  entries in the {\it 
followpos()} set.
\item[union(Left,Right):] Denotes a non-leaf node corresponding to the
$\mid$ operator in a regular expression, with {\tt Left}  and {\tt Right}
as the regular expressions to the left and right of the $\mid$ operator.
\item[concat(Left,Right):] Denotes a non-leaf node corresponding to the
concatenation of two regular expressions {\tt Left}  and {\tt Right}.
The concatenation is implicit in a regular expression specification.
\item[closure(Child):] Denotes a non-leaf node corresponding to the primitive
Kleene closure  operator($*$). Since this is a unary operator it has
only one child.
\item[iterate(Child,From,To):] 
Denotes a non-leaf node corresponding to the non-primitive iteration
operator \{\}. {\tt Child} refers to the regular expression
being iterated, while {\tt From}, and {\tt To} are iteration amounts.
\end{description}

The use of leaf nodes with character classes and the use of
{\tt iterate/3} nodes are added as extensions to  the
algorithm in the Ullman book to control explosion in size of the
regular expression syntax tree, and the followpos set. These in turn
influence the time taken for DFA construction.

The syntax tree construction phase builds only the tags. The {\tt node/4}
data structure is built in the second phase where the attributes are computed.
\begin{figure}[htb]
{\picture(300,150)(0,0)
   \put(152,150){\bf rule}
   \put(150,145){\line(-1,-1){34}}
   \put(170,145){\line(1,-1){34}}
   \put(160,145){\line(0,-1){32}}

   \put(100,100){\bf union}
   \put(145,100){\bf Result}
   \put(200,100){\bf Goal}

% branches under union
   \put(105,95){\line(-1,-1){31}}
   \put(115,95){\line(1,-1){31}}

   \put(60, 55){\bf leaf(1,\_)}
   \put(135,55){\bf concat}

% branches under concat
   \put(145,50){\line(-1,-1){30}}
   \put(155,50){\line(1,-1){30}}

   \put(100,10){\bf leaf(2,\_)}
   \put(160,10){\bf leaf(3,\_)}
\endpicture}
\caption{The Regular Expression Syntax Tree for {\tt 1|23}}
\end{figure}
**/
parse_plex_rules((R;Rs), [ParseTree0|ParseTree]) :-
   !,
   parse_plex_rule(R, ParseTree0),
   parse_plex_rules(Rs, ParseTree).
parse_plex_rules(R, [ParseTree]) :- 
   parse_plex_rule(R, ParseTree).

/**
\predindex{parse\_plex\_rules/2}
\predindex{parse\_plex\_rule/2}
The rules below are to take care of patterns
that can be within single quotes (') and double quotes ('').
\newpage
**/
parse_plex_rule(X is Result if Goal0, 
                rule(ParseTree,Result,Goal)) :- 
   !,
   (atom(X) -> name(X, XL) ; X = XL),
   parse_reg_expn(ParseTree, XL, []),
   process_goal(Goal0, Goal).

parse_plex_rule(X is Result, rule(ParseTree,Result,true)) :- 
   !,
   (atom(X) -> name(X, XL) ; X = XL),
   parse_reg_expn(ParseTree, XL, []).

parse_plex_rule(X, rule(ParseTree, XA, true)) :-
   (atom(X) -> X = XA, name(X, XL)
   ; X = [_|_], X = XL, name(XA, X)
   ),
   parse_reg_expn(ParseTree, XL, []),
   !.
parse_plex_rule(X, _, _)  :- 
    user:message_hook('lexicon_error'(X), _, _).

/**
\predindex{parse\_plex\_rule/2}
\predindex{parse\_reg\_expn/3}
\predindex{process\_goal/2}
\predindex{parse\_plex\_rule/3}
\predindex{message\_hook/3}
The regular expression that is part of a lexical rule
is parsed by invoking {\tt parse\_reg\_expn//1}. This is the head clause
of the DCG. The hidden input character list (in the DCG)  is the regular
expression that appears on one line, or until a semicolon. The lone argument
of the DCG returns the parse tree for the regular expression.

In its most simplistic form, this grammar is ambiguous, has left-recursion,
and is not-$\epsilon$ free. So  the grammar is first rewritten
to express precedence and associativity of the various
regular expression operators. These obey the same rules as in {\tt lex}.
The $\epsilon$ production is removed,
and then left recursion is removed. The grammar is expressed with non-primitive
regular expression operators like $+, ?$ etc. These are rewritten
during the course of the tree building process into primitive operators.

Having constructed a syntax tree representing the regular expression,
at the rule level, each such   tree is
augmented by appending a right-end marker {\tt accept} to it. 

The attributes {\tt Nullable}, {\tt First} and {\tt Last} and {\tt Pos} 
are not computed
in this first phase of parse tree construction. The parse tree is
constructed  with only the tags.

[{\tt NOTE}: Placeholders (to be filled in the 2nd phase) cannot be left 
for
the attributes  in the parse tree, because some regular expressions
are duplicated at nodes. For example $a+$  is $aa*$. While
positions are not computed until a later predicate, the unbound variables left
as placeholders  are unified  to the same (unbound)
value in both nodes. This is unacceptable because we require distinct position 
numbers for every non-$\epsilon$ node.]
**/

parse_reg_expn(Tree) --> 
   reg_expn(S),
   opt_parse_reg_expn(S, Tree).

/**
\predindex{parse\_reg\_expn//1}
\predindex{reg\_expn//1}
\predindex{opt\_parse\_reg\_expn//2}
End of line regular expression. 
**/
opt_parse_reg_expn(PTree, concat(concat(PTree,leaf(10)),leaf(accept))) -->
   "$", !.

opt_parse_reg_expn(PTree, concat(PTree,leaf(accept))) --> [].
/**
\predindex{opt\_parse\_reg\_expn//2}
Beginning of line regular expression.
**/
reg_expn(concat(leaf(10),R)) -->
    "^", 
    !,
    reg_expn_1(R).

reg_expn(R) -->
    reg_expn_1(R1),
    opt_reg_expn(R1, R).

/**
\predindex{reg\_expn//1}
\predindex{reg\_expn\_1//1}
\predindex{opt\_reg\_expn//2}
The union operator has the lowest precedence.
**/
opt_reg_expn(R1, R) -->
   "|",
    !,
   reg_expn_1(R2),
   opt_reg_expn(union(R1,R2), R).
opt_reg_expn(R, R) --> [].

	       
reg_expn_1(R1) -->
   reg_expn_2(R2),
   opt_reg_expn_1(R2, R1).
/**
\predindex{opt\_reg\_expn//2}
\predindex{reg\_expn\_1//1}
\predindex{reg\_expn\_2//1}
\predindex{opt\_reg\_expn\_1//2}
As mentioned earlier, concatenation is not represented explicitly in the
regular expression, we use the {\tt concat} tag to denote this node type
in the tree.
**/
opt_reg_expn_1(R1, R) -->
   reg_expn_2(R2),
   opt_reg_expn_1(concat(R1,R2), R).
opt_reg_expn_1(R, R) --> [].


reg_expn_2(R2) -->
   reg_expn_3(R3),
   opt_reg_expn_2(R3, R2).

opt_reg_expn_2(R, iterate(R,NumFrom,NumTo)) -->
    "{",
     repeated_occurance(From, To),
     {
     number_chars(NumFrom, From),      
     number_chars(NumTo, To)
     }.
opt_reg_expn_2(R, R) -->[].

repeated_occurance(From, To) -->
     occurance_count(From),
     opt_repeated_occurance(To).

opt_repeated_occurance(To) -->   occurance_count(To).   
opt_repeated_occurance([]) --> [].

occurance_count([]) --> ",".
occurance_count([]) --> "}".
occurance_count([C|Cs]) -->
    [C],
    {digit(C)},
    !,
    occurance_count(Cs).

reg_expn_3(R3) -->
   reg_expn_4(R4),
   opt_reg_expn_3(R4, R3).
/**
\predindex{opt\_reg\_expn\_1//2}
\predindex{reg\_expn\_2//1}
\predindex{reg\_expn\_3//1}
\predindex{opt\_reg\_expn\_2//2}
\predindex{repeated\_occurance//2}
\predindex{occurance\_count//1}
\predindex{opt\_repeated\_occurance//1}
\predindex{digit/1}
\predindex{reg\_expn\_4//1}
\predindex{opt\_reg\_expn\_3//2}
The following non-primitive (postfix) operators are replaced
by their primitive equivalents. They are higher in priority than
concatenation and union.
**/
opt_reg_expn_3(R3, R) -->
   "+", 
    !,
   opt_reg_expn_3(concat(R3,closure(R3)), R).
opt_reg_expn_3(R3, R) -->
   "?",
    !,
   opt_reg_expn_3(union(R3,leaf(empty)), R).

/**
\predindex{opt\_reg\_expn\_3//2}
The node of type {\tt closure} ($*$) is constructed here.
**/

opt_reg_expn_3(R3, R) -->
   "*", 
    !,
   opt_reg_expn_3(closure(R3), R).
opt_reg_expn_3(R, R) --> [].

/**
\predindex{opt\_reg\_expn\_3//2}
Character class notation [a-zA-Z] in a regular expression is captured
with a {\tt leaf/1} tag, where the argument is a character
class and not a single character symbol.  A class is not
represented using the  {\tt union} tag.
Other non-primitive operators include the following.

\begin{enumerate}
\item Complemented Character Classes, {\it e.g.:} \verb@[^...]@
\item A simple Character Class, {\it e.g.} [A-Z],
\item The ``.'', implying any character but a newline
\item Escaped characters which are used as PLEX operators in
their normal form, {\it e.g}, \verb@\+, \?@.
\item A single ASCII(0-128) character (that is non-special). Special characters
are {\tt $\mid$, *, +, ?, \$, \(, \), \{, \}}.
\end{enumerate}

The complemented character class and the ``.'' operator are
also parsed as {\tt leaf/1} nodes with character classes as arguments.
**/

reg_expn_4(leaf(Syms)) -->
   "[", "^",
    !,
   range(Syms0),
   {sort(Syms0, Syms1),
    between(1, 128, AllSyms, []),
    new_offset(AllSyms, Syms1, Syms)}.
/**
\predindex{reg\_expn\_4//1}
\predindex{range//1}
\predindex{between/4}
\predindex{new\_offset/3}
**/
reg_expn_4(leaf(Range)) -->
    "[",  !, 
     range(Range0), 
     {sort(Range0, Range)}.

reg_expn_4(leaf(C)) -->
    ".", !,
    {between(1, 9, C, C1),
    between(11, 128, C1, [])}.

reg_expn_4(R) -->   "(",!, reg_expn(R),   ")".

reg_expn_4(leaf(Sym)) -->
    escapedChar(Sym),
    !.
reg_expn_4(leaf(R)) -->
     [R], {terminal_node(R)},!.


escapedChar(127)--> "\d", !. % Delete
%escapedChar(32)--> " ",  !. % Space Bar
escapedChar(27) --> "\e", !. % ESC
escapedChar(13) --> "\r", !. % carriage return
escapedChar(12) --> "\f", !. % form feed
escapedChar(11) --> "\v", !. % vertical tab
escapedChar(10) --> "\n", !. % new line
escapedChar(9) --> "\t",  !. % tab
escapedChar(8) --> "\b",  !. % backspace
escapedChar(7) --> "\a",  !. % alarm =BEL

escapedChar(empty) --> "\c",!. % no character
escapedChar(empty) --> [0'\,C], {layout_char(C)},!. %no character
escapedChar(Code) -->  [0'\,^,C], {Code is C mod 32},!.
escapedChar(C) --> [0'\,C], !.

terminal_node(C) :-   C > 32 , \+ special(C).
layout_char(C) :- C =< 32, C >= 127.

special(0'|).
special(0'*).
special(0'+).
special(0'?).
special(0')).
special(0'().
special(0'{).
special(0'}).
special(0'$).
/**
\predindex{reg\_expn\_4//1}
\predindex{range//1}
\predindex{between/4}
\predindex{reg\_expn//1}
\predindex{escapedChar//1}
\predindex{terminal\_node/1}
\predindex{layout\_char/1}
\predindex{special/1}
**/

range([]) --> "]", !.

range(R) --> [A, 0'-, Z],
      { Z =\= 0'], !, between(A, Z, R, R0) },
      range(R0).
range([R|T]) --> escapedChar(R), !, range(T).
range([R|T]) --> [R], range(T).

/**
\predindex{range//1}
\predindex{between/4}
\predindex{escapedChar//1}
The predicate below scans through the action (left hand side)
of the rule, searching and replacing escaped characters
with their ascii character codes. It calls
{\tt process\_chars/3} on each ASCII list that it finds.
**/
process_goal(Var,Var) :- var(Var),!.
process_goal([], []) :- !.
process_goal([H|T], [NH|NT]) :-
    is_ascii_list([H|T]),
    !,
    process_chars([NH|NT], [H|T], []).
process_goal([H|T], [NH|NT]) :-
    process_goal(H,NH),
    !,
    process_goal(T, NT).
process_goal(G, NG) :-
    process_term(G, NG).

process_chars([]) -->  [].
process_chars([Ch|Chs]) -->  
    process_char(Ch),
    process_chars(Chs).

process_char(Ch) -->   escapedChar(Ch),!.
process_char(Ch) -->   [Ch], {is_char(Ch)},!.

is_ascii_list(*) :- !, fail.  % Catch variables
is_ascii_list([]).
is_ascii_list([H|T]) :-
     nonvar(H),
     is_ascii(H),
     is_ascii_list(T).

process_term(Term, NewTerm) :-    
    Term =.. [Functor|List],
    !,
    process_goal(List, NewList),
    NewTerm =.. [Functor|NewList].

/**
\predindex{process\_goal/2}
\predindex{is\_ascii\_list/1}
\predindex{process\_chars/3}
\predindex{process\_term/2}
\predindex{process\_chars//1}
\predindex{process\_char//1}
\predindex{escapedChar//1}
\predindex{is\_char/1}
\predindex{is\_ascii/1}
\subsection{Computing Attributes and Annotating the Syntax Tree}
At this stage the regular expression specification has been
parsed and the syntax tree has been constructed. The tree is only made up
of nodes tagged with different types. In this section the attributes are
 computed and the {\tt node/4} data structure is built.
\vskip 0.2cm
\centerline{\tt node(Tag,Nullable,First,Last) }
\vskip 0.2cm

The syntax tree for the PLEX specification is a {\it list} of {\tt rule/3}
terms.  The first argument of each term is a syntax tree denoting the parsed
regular expression. 
This tree is traversed, the non-$\epsilon$ symbols
are assigned {\bf unique} position numbers, and the functions
{\it nullable(), firstpos(),} and {\it lastpos()}
are computed and marked as attributes of nodes in the
parse tree. An attributed syntax tree {\tt node/4} is thus constructed
for each PLEX rule.

This tree is then traversed a second time to
compute the {\it followpos()} function for each position number
in the tree. At the same time, the start states (which are sets of positions)
and the  symbol-position pairs  are also computed. The symbol-position
pairs collect all the positions associated with a given character symbol.
**/
annotate_parsetree([Rule|Rules], SymPosPrs, StartSt, fp(IndexKeys,FPValues)) :-
    annotate_parsetree_1([Rule|Rules], SymPosPrs0, [], StartSt0, FPSet),
    keysort_union(SymPosPrs0, SymPosPrs),
    ord_union(StartSt0, StartSt),
    keys_and_values(FPSet, IndexKeys, FPValues).

annotate_parsetree_1([], Z, Z, [], []).
annotate_parsetree_1([rule(SynTree, Tok, Act)|Rules], 
		     SymPosPairs0, SymPosPairs,
                     [StartSt|StartSts], [FPSet|FPSets]) :-
    compute_null_fpos_lpos(SynTree, final_st(Tok,Act), AttrSynTree),
    compute_followpos(AttrSynTree, SymPosPairs0, SymPosPairs1, FPSet),
    node_first(AttrSynTree, StartSt),
    annotate_parsetree_1(Rules, SymPosPairs1, SymPosPairs, StartSts, FPSets).

compute_null_fpos_lpos(union(Lc,Rc), TokAction,
                       node(union(Lc0,Rc0),Nullable,First,Last)):-
    compute_null_fpos_lpos(Lc, TokAction, Lc0),
    compute_null_fpos_lpos(Rc, TokAction, Rc0),
    node_attrs(Lc0, LcNullable, LcFirst, LcLast),		    
    node_attrs(Rc0, RcNullable, RcFirst, RcLast),		
    bool_or(RcNullable, LcNullable, Nullable),
    ord_union(LcFirst, RcFirst, First),
    ord_union(LcLast, RcLast, Last).

compute_null_fpos_lpos(concat(Lc,Rc), TokAction,
                       node(concat(Lc0,Rc0),Nullable,First,Last)) :-
    compute_null_fpos_lpos(Lc, TokAction, Lc0),
    compute_null_fpos_lpos(Rc, TokAction, Rc0),
    node_attrs(Lc0, LcNullable, LcFirst, LcLast),		    
    node_attrs(Rc0, RcNullable, RcFirst, RcLast),		
    bool_and(RcNullable, LcNullable, Nullable),
    nullable(LcNullable, LcFirst, RcFirst, First),
    nullable(RcNullable, RcLast,  LcLast,  Last).

compute_null_fpos_lpos(closure(Child), TokAction,
                       node(closure(Child0), t,First,Last)) :-
    compute_null_fpos_lpos(Child, TokAction, Child0),
    node_attrs(Child0, _, First, Last).
/**
\predindex{annotate\_parsetree/4}
\predindex{compute\_null\_fpos\_lpos/3}
\predindex{compute\_followpos/4}
\predindex{node\_first/2}
\predindex{keysort_union/3}
\predindex{annotate\_parsetree\_1/5}
\predindex{ord\_union/2}
\predindex{keys\_and\_values/3}
\predindex{extract\_attrs/4}
\predindex{bool\_or/3}
\predindex{ord\_union/3}
\predindex{bool\_and/3}
The {\tt iterate/3} node in the syntax tree indicates
that the regular expression denoted by {\tt Child} can to be repeated 
from {\tt M} times to a maximum of {\tt N} times.
This is denoted in the syntax tree
as follows: First the {\tt concat/2} tag is used to repeat the regular
expression {\tt M} times, yielding {\tt Child0}. Then instead
of continuing this process to generate {\tt N-M+1} concatenated copies of
{\tt Child} with sizes {\tt M+1, M+2}, ... {\tt N}
and combining them all with the {\tt union/2} tag, we
create  {\tt N-M+1} concatenated copies of {\tt Child}
but all of size {\tt M} and combine them with {\tt union/2}.
The {\tt M+1, M+2...N} copies  share their last {\tt M} copies
from the first set. For a regular expression in {\tt Child} of size
$r$, this yields a savings of $r*(n-m+1)$ nodes in the syntax tree
which translate into savings in the {\tt followpos()} set and
DFA construction time.
**/
%
% THE iterate clause of this PREDICATE HAS NOT BEEN TESTED YET (Jun 4 1992)
%
%
compute_null_fpos_lpos(iterate(Child,M,N), TokAction, Child2) :-
    IterCnt0 is N - M,
    IterCnt is IterCnt0 + 1,
    generateMcopies_ofchild(Child, 1, M, Child0),
    generate_iter_nodes(Child0, 1, IterCnt, Child1),
    compute_null_fpos_lpos(Child1, TokAction, Child2).

compute_null_fpos_lpos(leaf(empty), _,
                       node(leaf(empty,_),t,[],[])) :-!.
compute_null_fpos_lpos(leaf(accept), TokAction,
                       node(leaf(accept,accept(Pos,TokAction)),
                            f,[Pos],[Pos])) :-
   !,
   ctr_is(0, Pos), 
   ctr_inc(0).

% C denotes a single character or a character class
compute_null_fpos_lpos(leaf(C), _,
                       node(leaf(C,Pos),f,[Pos],[Pos])) :-
   ctr_is(0, Pos), 
   ctr_inc(0).

nullable(t, A, B, C) :- ord_union(A,B,C).
nullable(f, A, _, A).
/**
\predindex{compute\_null\_fpos\_lpos/3}
\predindex{generateMcopies\_ofchild/4}
\predindex{nullable/4}
\predindex{generate\_iter\_nodes/4}
\predindex{ctr\_is/2}
\predindex{ctr\_inc/1}
NOT TESTED YET (June 4 92)
**/
generateMcopies_ofchild(Child, Cnt, Cnt, Child) :-!.
generateMcopies_ofchild(Child, M0, M, concat(Child,Children)) :-
    M1 is M0 + 1,
    generateMcopies_ofchild(Child, M1, M, Children).

generate_iter_nodes(Child, IterCnt, IterCnt, Child) :-!.
generate_iter_nodes(Child, IterCnt0, IterCntN, union(Child,Children)) :-
    IterCnt1 is IterCnt0 + 1,
    generate_iter_nodes(Child, IterCnt1, IterCntN, Children).

/**
\predindex{generateMcopies\_ofchild/4}
\predindex{generate\_iter\_nodes/4}
Every node has 3 attributes: {\tt Nullable,} {\tt FirstPos, LastPos}.
**/

node_attrs(node(_,Nullable,First,Last), Nullable, First, Last).

node_last(    node(_,_,_,L), L).
node_first(   node(_,_,F,_), F).
node_nullable(node(_,N,_,_), N).

bool_and(t,t,X) :- !, X = t.
bool_and(_,_,f).

bool_or(f,f,X)  :- !, X = f.
bool_or(_,_,t).
/**
\predindex{bool\_and/3}
\predindex{bool\_or/3}
The predicate {\tt compute\_followpos\_1/5} returns the symbol position
pairs appearing in a single regular expression tree as a difference list. The
follow position list for this tree is returned with the very first position
number
prefixed as an index key. So if the current tree had positions from 11 to 29.
Then, the follow position set would be $11-[11-FPsOf11, 12-FPsOf12,...]$
This index key makes it easy to search through a large follow position
set.
**/
compute_followpos(node(Tag,_,_,_),
       SymPosPrs0, SymPosPrs, Pos-[Pos-FP|FPSet]):-
    compute_followpos_1(Tag, SymPosPrs0, SymPosPrs,
                         FollowPosPairs, []),
    keysort_union(FollowPosPairs, [Pos-FP|FPSet]).
/**
\predindex{compute\_followpos/4}
\predindex{compute\_followpos\_1/5}
\predindex{keysort_union/2}
The first clause for the {\tt leaf/2} tag handles character classes,
while the second clause handles the {\tt accept} marker (representing
accepting states) and single character symbols.
**/
compute_followpos_1(leaf(Sym,Pos),[Sym-[Pos]|SpPrs],SpPrs,FpPrs,FpPrs).
compute_followpos_1(union(node(LeftC,_,_,_),node(RightC,_,_,_)), 
                    SpPrs0, SpPrs, FpPrs0, FpPrs) :-
    compute_followpos_1(LeftC, SpPrs0, SpPrs1, FpPrs0, FpPrs1),
    compute_followpos_1(RightC, SpPrs1, SpPrs, FpPrs1, FpPrs).

compute_followpos_1(concat(node(LeftC,_,_,LastL),
                           node(RightC,_,FirstR,_)),
                    SpPrs0, SpPrs, FpPrs0, FpPrs):-
    update_fpos_pairs(LastL, FirstR, FpPrs0, FpPrs1),
    compute_followpos_1(LeftC, SpPrs0, SpPrs1, FpPrs1, FpPrs2),
    compute_followpos_1(RightC, SpPrs1, SpPrs, FpPrs2, FpPrs).

compute_followpos_1(closure(node(Child,_,First,Last)),
                    SpPrs0, SpPrs, FpPrs0, FpPrs) :-
    update_fpos_pairs(Last, First, FpPrs0, FpPrs1),
    compute_followpos_1(Child, SpPrs0, SpPrs, FpPrs1, FpPrs).

/**
\predindex{compute\_followpos\_1/5}
\predindex{update\_fpos\_pairs/4}
The code for followpos computation on an iterate node
has not been written yet.
**/
%
%compute_followpos_1(iterate(node(Child,_,First,Last), From, To),
%                    SpPrs0, SpPrs, FpPrs0, FpPrs) :-


/**
**/
update_fpos_pairs([], _, FpPrs, FpPrs).
update_fpos_pairs([Posn|Posns], FPsToAdd,
             [Posn-FPsToAdd|FpPrs1], FpPrs) :-
    update_fpos_pairs(Posns, FPsToAdd, FpPrs1, FpPrs).
    
/**
\predindex{update\_fpos\_pairs/4}
\subsection{Constructing the DFA}
The predicate below constructs the deterministic finite automaton that 
recognizes the tokens for the given PLEX specification.
It accepts as input the name of the language, the start state of
the DFA, the follow position set, and the ordered 
set  of character symbol position
 pairs. The follow position set is actually an ordered set of sets with each
individual set denoting the follow position for one PLEX rule.

To start with, the  positions associated with final state denoted
by the symbol {\tt accept} are extracted from the symbol position pair set.

The predicate {\tt generate\_dfa\_states/8} is then
invoked. It implements the algorithm
described earlier, returning a list of quadruples
representing the state transition table:
\vskip 0.2cm
\centerline{\tt table( CurSt, Sym, NxtSt, Action)}
\vskip 0.2cm

Where {\tt CurSt}, and {\tt NxtSt} are lists of positions representing the
current state, and next state, respectively.
{\tt Sym} is the input alphabet symbol on which the transition is
taking place. If {\tt Action}   is {\tt "null"},
then it means  {\tt NxtSt} is not a final state.
If {\tt NxtSt} is a final state, then {\tt Action}
is a  term of the type {\tt final\_state(Token,Goal)},
where {\tt Token} is the token being returned and
{\tt Goal} is the action to be performed.

The predicate {\tt generate\_dfa\_states/8} returns a Set of Marked States
({\tt MarkedSts}) and the State Table {\tt StateTable0}.
{\tt MarkedSts} is actually a set of
pairs, where the first element of the pair is the list of positions denoting
a state, and the second element is a name for that state, constructed
with a language prefix.  For {\it e.g.} [1, 2, 4, 7] and [6, 8, 5] become
{\tt [1, 2, 4, 7]-xyz} and  {\tt [6, 8, 5]-xyz\_1}, respectively, for
language {\tt xyz}.
The state table is rewritten with the state names. 
**/

construct_dfa(Name, StartSt, SymPosSet0, 
              FollowPosSet, StateTable) :-
    del_element(accept-FinalStPosns, SymPosSet0, SymPosSet1),
    (verbose ->
        print_length_of_longest_PosSet(SymPosSet1),
        format(user_error,"SymPosSet....~n",[]),
%       write_list(SymPosSet1),
        format(user_error,"FinalStPosns.~n",[])
%       write_list(FinalStPosns)
    ; true
    ),
    statistics(runtime,[T0|_]),
    generate_dfa_states([StartSt], data(SymPosSet1,FinalStPosns,FollowPosSet),
                        Name, 0, [], [], MarkedSts, StateTable0),
    statistics(runtime,[T1|_]),
    T is T1 -T0,
    vformat(user_error,"took ~3d sec to gen states.~n",[T]),
    vformat(user_error,"Updating State Table Names...~n",[]),
    update_state_table_names(StateTable0, MarkedSts, StateTable),
    length(StateTable, StSize),
    vformat(user_error,"State Table Entries..~d,~n",[StSize]).
/**
\predindex{construct\_dfa/5}
\predindex{del\_element/3}
\predindex{verbose/0}
\predindex{print\_length\_of\_longest\_PosSet/1}
\predindex{generate\_dfa\_states/8}
\predindex{vformat/3}
\predindex{update\_state\_table\_names/3}
\subsubsection*{State Generation}
The next two predicates make up the  heart of the DFA construction algorithm.
The program spends almost  all its time  in them.

For the sake of convenience, we reproduce the steps in this predicate,
{\tt generate\_dfa\_states/8}.
It may be recalled a state in the DFA is really a set of positions.
\begin{verbatim}
While there are no unmarked states do
     1. Pick one state (CurSt) from the Unmarked state set.
     2. Create a State Name for it and add to the Marked Set.
     3. Get the set of New Unmarked States generated from CurSt,
        due to transitions on all input symbols. Data is the
        data structure that holds the symbol position pairs list.
     4. If no new unmarked states are generated, then mark CurSt as
        a final state.
 end while
\end{verbatim}

The predicate {\tt get\_new\_unmarkedSts\_1/9} executes Step 3
and {\tt update\_poss\_finalst\_1/5} executes Step 4, of above.
**/
%
%generate_dfa_states(+ UnmrkdStSet, + Data, + StateNamePrefix, + StateCnt,
%                    + MarkedSts, + StTable, - FinalMarkedSts, - FinalStTable)
%   
generate_dfa_states([], _, _, N, MarkdSts, Table, MarkdSts, Table) :-
   vformat(user_error,"Total # of States:..~p~n",[N]).
generate_dfa_states([CurSt|UnMrkdStSet0], Data, Lang, N0,
                     MarkdSts0, StateTable0, MarkdSts, StateTable):-
   create_state_name(N0, Lang, StateName),
   N1 is N0 + 1,
   add_element(CurSt-StateName, MarkdSts0, MarkdSts1),
   get_new_unmarkedSts(CurSt, Data, MarkdSts1, UnMrkdStSet0, UnMrkdStSet1,
                       StateTable0, StateTable1),
   length(UnMrkdStSet1, Length),
   vformat(user_error,"~d Unmarked States Generated ~n",Length),
   update_poss_finalst(StateTable0, StateTable1, Data, CurSt, StateTable2),
   generate_dfa_states(UnMrkdStSet1, Data, Lang, N1, MarkdSts1, StateTable2,
		       MarkdSts, StateTable).

create_state_name(0, Lang, Lang) :- !.
create_state_name(N, Lang, StateName) :-
   concat_atom([Lang, '_', N], StateName).
/**
\predindex{generate\_dfa\_states/8}
\predindex{vformat/3}
\predindex{create\_state\_name/3}
\predindex{add\_element/3}
\predindex{get\_new\_unmarkedSts/7}
\predindex{update\_poss\_finalst/5}
\predindex{concat\_atom/2}
The following predicate goes through the list of symbols
and computes all the unmarked states reachable by
a transition of some symbol from the {\tt CurSt}.
As mentioned earlier the {\tt CurSt} is a list of positions.
Its steps can be paraphrased below.
For every symbol s in the Symbol Position List 
\begin{enumerate}
\item Intersect the Positions in CurSt, with the Positions
     associated with symbol s.
\item For each Position in the Intersected Set,{\tt CurStPosns}, collect
        the set of follow positions, and take the union of
        all these follow positions. This yields a NewState,
        with transition s from CurSt: table(CurSt,NewState,\_).
\item Add this entry to  the State Table.
\item Search through the Unmarked State List to see if it needs
        to be added
\end{enumerate}

{\tt get\_candidate\_nextSt/9} is called to perform Step 2 above,
while  Steps 3 and 4 are performed  by the two update predicates.
**/
get_new_unmarkedSts(CurSt, data(SPSet,FStPos,FPSet), MarkdSts, 
                           UnMrkdStSet0, UnMrkdStSet,
                           StateTable0, StateTable) :-
   get_new_unmarkedSts_1(SPSet, FStPos, FPSet, CurSt, MarkdSts, 
                         UnMrkdStSet0, UnMrkdStSet, StateTable0, StateTable).

get_new_unmarkedSts_1([], _, _, _, _, UnM, UnM, Table, Table).
get_new_unmarkedSts_1([empty-_|SPS], FSP, FPSt, CSt, M, U0, U, T0, T):-  !,
    get_new_unmarkedSts_1(SPS, FSP, FPSt, CSt, M, U0, U, T0, T).
get_new_unmarkedSts_1([Sym-Posns|SPSet], FStPos,
                      FPSet, CurSt, MarkdSts,
                      UnMStSet0, UnMStSet, StTable0, StTable) :-
    ord_intersection(Posns, CurSt, CurStPosns),
    ( CurStPosns == [] ->
        get_new_unmarkedSts_1(SPSet, FStPos, FPSet, CurSt,
                              MarkdSts, UnMStSet0, UnMStSet,
                              StTable0, StTable)
    ; get_candidate_nextSt(CurStPosns, FPSet, NxtSt),
      update_stTable(NxtSt, CurSt, Sym, FStPos, StTable0, StTable1),
      update_unMSet_ifneeded(NxtSt, MarkdSts, UnMStSet0, UnMStSet1),
      get_new_unmarkedSts_1(SPSet, FStPos, FPSet,
                            CurSt, MarkdSts, UnMStSet1, UnMStSet,
                            StTable1, StTable)
    ).

/**
\predindex{get\_new\_unmarkedSts/7}
\predindex{get\_new\_unmarkedSts\_1/9}
\predindex{ord\_intersection/3}
\predindex{get\_candidate\_nextSt/3}
\predindex{update\_stTable/6}
\predindex{update\_unMSet\_ifneeded/4}
The predicate below collects the list of positions that make up
the next state. Its input is a set {\tt [P|Posns]}
holding  the list of positions common to the current state  and associated
with the current symbol, and the follow position set.

The follow position set, is actually a term made up of an {\tt Index}
and the set itself. The {\tt FPSet} is an ordered set of
sets, where each set element consists of the  follow position set for
all the positions created in a given PLEX rule.
The {\tt Index} is the list of starting position numbers in each set
element.  This makes it easy to do a binary search through the {\tt Index}
to quickly determine the {\tt FPSetElem} that is likely to contain 
the follow position set for the given position P. The followpos of P
is then obtain via sequential search in this {\tt FPSetElem}.

By taking the union of the {\it follow position} for all {\tt [P|Posns]},
a new set of positions is generated. This is the next state.
If a position does not have a follow position, then it is part of a
final state, and so does not have a next state.  So we just skip over it.
**/
get_candidate_nextSt([], _, []).
get_candidate_nextSt([P|Posns], fp(Index, FPSet), Candidate) :-
    index_and_retrieve_SetElem(P, Index, FPSet, FPSetElem),
    ( memberchk(P-FPosns, FPSetElem) ->
         get_candidate_nextSt(Posns, fp(Index, FPSet), FPosns, Candidate)
    ; get_candidate_nextSt(Posns, fp(Index, FPSet), Candidate)
    ).


get_candidate_nextSt([], _, Candidate, Candidate).
get_candidate_nextSt([P|Posns], fp(Index, FPSet), FPosns0, Candidate) :-
    index_and_retrieve_SetElem(P, Index, FPSet, FPSetElem),
    ( memberchk(P-FPosns1, FPSetElem) ->
       ord_union(FPosns0, FPosns1, FPosns2),
       get_candidate_nextSt(Posns, fp(Index, FPSet), FPosns2, Candidate)
    ; get_candidate_nextSt(Posns, fp(Index, FPSet), FPosns0, Candidate)
    ).

/**
\predindex{get\_candidate\_nextSt/3}
\predindex{index\_and\_retrieve\_SetElem/4}
\predindex{memberchk/2}
\predindex{get\_candidate\_nextSt/4}
\predindex{ord\_union/3}
Binary search through the Index list to retrieve the index number of the
{\tt FPSet} element that is likely to contain the follow positions {\tt Posn}.
Then use the {\tt IndexNum} to retrieve the set element from the {\tt FPSet}.

Note: I (Peter) am convinced that this code is not only
unnecessary, but rather inefficient. Binary searching on a list
makes no sense to me whatsoever. I'm waiting to hear from Suresh
that I am right, since I haven't fully unraveled what is going
on here (particularly when Posn does not match a particular index).
**/

index_and_retrieve_SetElem(_, Index, FPSet, FPSet) :-
    integer(Index), !.
index_and_retrieve_SetElem(Posn, IndexList, FPSet, FPSetElem) :-
    length(IndexList, N),
    retrieve_index_number(1, N, Posn, IndexList, IndexNum),
    nth1(IndexNum, FPSet, FPSetElem).


retrieve_index_number(Lower, Lower, _, _, Lower) :-     !.
retrieve_index_number(Lower, Upper, Posn, IndexList, IndexNum) :-
%     Lower < Upper   % Lower - Upper > 1
      Mid is Lower + Upper,
      Mid0 is Mid // 2,
      nth1(Mid0, IndexList, IndexElem0),
      Mid1 is Mid0 + 1,
      nth1(Mid1, IndexList, IndexElem1),
      (Posn == IndexElem0 ->
           IndexNum = Mid0
      ; Posn == IndexElem1 ->
           IndexNum = Mid1
      ; Posn > IndexElem0 -> %Posn could still be in Mid0 if < IndexElem1
	    (Posn < IndexElem1 ->
              IndexNum = Mid0
             ; Lower1 is Mid1,
	      retrieve_index_number(Lower1, Upper, Posn, IndexList, IndexNum)
	    )
      ; Posn < IndexElem0 ->  % Posn has to be in upper half of IndexList
             Upper1 is Mid0-1,
	     retrieve_index_number(Lower, Upper1, Posn, IndexList, IndexNum)
      ).

/**
\predindex{index\_and\_retrieve\_SetElem/4}
\predindex{retrieve\_index\_number/5}
\predindex{nth1/3}
Update the state table by adding the transition at the
head of the table.
**/
update_stTable([], _, _, _, StTable, StTable) :-!.
update_stTable(NxtSt, CurSt, Sym, FinalStPosns, StTable,
              [table(CurSt,Sym,NxtSt,TokAction)|StTable]):-
        member(accept(Posn,TokAction), FinalStPosns),
        memberchk(Posn, NxtSt),
        !.
update_stTable(NxtSt, CurSt, Sym, _, StTable,
              [table(CurSt,Sym,NxtSt,"null")|StTable]).
/**
\predindex{update\_stTable/6}
\predindex{memberchk/2}
\predindex{member/2}
If the generated Next state  has already been marked,
{\it i.e} it is a member of the marked state, then
the unmarked state set is not updated.
If not, the generated next state is added to the Unmarked state set.
Since this is a set, no duplicates are kept, and this prevents adding
states that have been generated but not marked yet.
**/
update_unMSet_ifneeded([],          _, UnMarked, UnMarked) :-
       !.
update_unMSet_ifneeded(States, Marked, UnMarked, UnMarked) :-
       memberchk(States-_, Marked),
       !.
update_unMSet_ifneeded(States, _, UnMarked0, UnMarked) :-
       ord_add_element(UnMarked0, States, UnMarked).
/**
\predindex{update\_unMSet\_ifneeded/4}
\predindex{memberchk/2}
\predindex{ord\_add\_element/3}
This predicate is called after {\tt get\_new\_unmarked\_state}
is called. It compares the state table ({\tt StTable1}) returned
from this predicate and the original state table ({\tt StTable0}).
If  new states {\it or} transitions were generated in this predicate,
then  the heads of the two state tables would be different.
If this were not the  case, then we check if 
the current state {\tt CurSt} could be a possible  final state.
If it is, then return
the token action pair via {\tt TokAction}.

The reason the {\tt nil,nil} is needed is because 
{\tt CurSt} is a final state and has no next state and input character
transition. Usually a state is marked as a final state when a transition
is made to it. Then the token is returned and the goal is executed 
on all the characters on which final state fails to make a transition.

In this case this additional entry is needed for
{\tt CurSt} because there is no input character transition from this state.
So in order  to generate a clause that returns the token and executes the
goal on all the characters  on which this state fails to make a transition,
this entry has to be made.
**/
update_poss_finalst([Hd|StTable0],[Hd|_], data(_,FinalSPos,_), CurSt,
                      [table(CurSt,nil,nil,TokAct),Hd|StTable0]):-
         member(accept(Posn,TokAct), FinalSPos),
         memberchk(Posn, CurSt),
         !.
update_poss_finalst(_, StTab, _, _, StTab).
/**
\predindex{update\_poss\_finalst/5}
\predindex{memberchk/2}
\predindex{member/2}
This contains the final state membership check to 
see if any position in the current state {\tt CurState}
is a member of the Final state position list.
The states obtained after constructing the DFA are
sets of positions and this predicate replaces them by state names.
Each state name is prefixed by the name of the language
(shown below as {\tt Name}). Successive states are numbered as
{\tt Name\_1, Name\_2} etc. After the states are named, the
predicate {\tt update\_state\_table\_names/3}
rewrites the state transition table with the new names.
**/
update_state_table_names([], _, []).
update_state_table_names([table(St,nil,nil,F)|Table],
                         NamedStates,
                         [table(NSt,nil,nil,F)|NTable]) :-
      !,
      memberchk(St-NSt, NamedStates),
      update_state_table_names(Table, NamedStates, NTable).
update_state_table_names([table(St1,C,St2,F)|Table],
                         NamedStates,
                         [table(NSt1,C,NSt2,F)|NTable]) :-
      memberchk(St1-NSt1, NamedStates),
      memberchk(St2-NSt2, NamedStates),
      update_state_table_names(Table, NamedStates, NTable).
/**
\predindex{update\_state\_table\_names/3}
\predindex{memberchk/2}
\subsubsection{ Top Level Clause}
This is the top level clause that calls the lexical analyzer and
generates the tokens. The Tokens returned can be lists or individual
tokens.
**/
lex_top_level(Name, 
               [(First :- !),
                (Second :- (GetToken,
                     (Token == EOF -> Tokens=[]
                     ; Token = [_|_] ->
                           append(Token,MoreTokens,Tokens)
                     ; Token == [] -> MoreTokens = Tokens
                     ; Tokens=[Token|MoreTokens]
                     ), NextCall)),
                (Third :- !),
                (Fourth :- Then) ]) :-
    EOF = ' $$ eND oF fILE ## ',
    First    =.. [Name,[],[]],
    Second   =.. [Name, Cs, Tokens],
    GetToken =.. [Name, Cs, Token, Rest],
    NextCall =.. [Name, Rest, MoreTokens],
    Third    =.. [Name, [], EOF, []],
    Fourth   =.. [Name, [C|InChars], OutToken, RestChars],
    Then     =.. [Name, C, InChars, T,T, _,OutToken, RestChars].

/**
\predindex{lex\_top\_level/2}
\subsection{Generating Clauses of the DFA}
**/
/**
The predicate {\tt generate\_clauses/4} is a top-level
clause that calls {\tt generate\_clauses/6} to generate 3 kinds of clauses:
\begin{enumerate}
\item {\it on} clauses.
When a  state transition from one state to
a non-final state is encountered (on a symbol), then the 
{\it on} clause is generated via {\tt gen\_on\_clause/5}.
\item  {\it on\_final} clauses.
When a state transition from one state to a final state 
is encountered (on a symbol), then an {\it on\_final} clause
is generated via {\tt gen\_on\_final\_clause/7}.  This predicate
replaces the current {\tt prev/3} term by a new {\tt prev/3} term.
The {\tt prev/3} term, as described earlier, is a one-cell cache that
remembers information about the most recent final state encountered.
This predicate also runs the goal predicates  through a simple processor
that recognizes various commands in the goal predicates. These commands
are simple predicates that perform tasks on the input character list.
This is discussed in {\tt sub\_builtins/4}.
\item {\it off} clauses. 
{\tt gen\_off\_clauses/4} is called for a set of symbols
on which there is no transition from the current state. In this case
the current {\tt prev/3} term is accessed to get the token to be returned
and the goal in the {\tt prev/3} term becomes the body. 
\end{enumerate}

The predicate works on a list of entries that make up the state transition
table. When these entries are generated (during DFA construction)
they are ordered such that all transitions from a given
state appear together. So this makes the task of detecting a 
``no further transition from current state (on a symbol)'' easy.

Until the state that matches  the {\tt gen\_off\_clauses/4} clause is
discovered the symbol set size is shrunk by 1 for each call of
{\tt generate\_clauses/6}.
The predicate {\tt gen\_off\_clauses/4} calls the top level
{\tt generate\_clauses/4} after generating the offset clauses.

The predicate  {\tt generate\_clauses/6} 
takes care of 4 types of state transition entries in the table.
\begin{enumerate}
\item The first {\tt table(St,nil,nil,\_)} is when
an accepting state has been encountered that has no transition 
on any symbol to any state. (The fact that it is an accepting state
has already been identified when {\tt St} appears at 
next state entry of the table. In this case, "off" clauses,
indicating  that the token needs to be returned, are generated
for transitions on all symbols from {\tt St}.
(See {\tt update\_poss\_finalst/5 } where this is generated).
\item {\tt table(CurSt,C,} {\tt NxtSt,"null")}, indicating a 
transition to a non-accepting state  with no action or token
\item {\tt table(CurSt,C,N,final\_st(T,G))}, indicating a
transition to an accepting state {\tt N}, on the symbol {\tt C}.
\item {\tt table(Name,C,N,final\_st(T,G))}. This is a sub case of
the previous one. If the current state is the start state, it
 is has  the same name as that of the language  {\tt  Name}.
In this case no ``off'' clause need be generated, because if there are
no transitions  on symbols other than {\tt C}, then it is an error.
\end{enumerate}
**/

generate_clauses([], _, Clauses, Clauses).
generate_clauses([table(CurSt,A,B,C)|Ts], Name, Clauses, Clauses1) :-
     between(1, 128, Set, []),
     !,
     generate_clauses([table(CurSt,A,B,C)|Ts], Set ,Name, CurSt,
		      Clauses, Clauses1).

generate_clauses([], _, Name, Name, Clauses, Clauses) :- !.
generate_clauses([], OffSet, _, CurSt, Clauses, Clauses1) :-
    gen_off_clauses(OffSet, CurSt, Clauses, Clauses1).

generate_clauses([table(CurSt,nil,nil,_)|Entries],
                  OffSet, Name, CurSt, Clauses0, Clauses) :-
    !,
    gen_off_clauses(OffSet, CurSt, Clauses0, Clauses1),
    generate_clauses(Entries, Name, Clauses1, Clauses).

generate_clauses([table(CurSt,C,NxtSt,"null")|Table],
                  OffSet, Name, CurSt, Clauses0, Clauses):-
     !,
     gen_on_clauses(C, CurSt, NxtSt, Clauses0, Clauses1),
     new_offset(OffSet, C, OffSet1),
     generate_clauses(Table, OffSet1, Name, CurSt, Clauses1, Clauses).

generate_clauses([table(CurSt,C,N,final_st(T,G))|Table],
                 OffSet, Name, CurSt, Clauses0, Clauses):-
     !,
     gen_on_final_clauses(C, CurSt, N, T, G, Clauses0, Clauses1),
     new_offset(OffSet,   C, OffSet1),
     generate_clauses(Table, OffSet1, Name, CurSt, Clauses1, Clauses).

generate_clauses(Entries, _, Name, Name, Clauses0, Clauses) :-
    !,
    generate_clauses(Entries,  Name, Clauses0, Clauses).

generate_clauses(Entries, OffSet, Name, CurSt, Clauses0, Clauses):-
    gen_off_clauses(OffSet, CurSt, Clauses0, Clauses1),
    generate_clauses(Entries, Name, Clauses1, Clauses).

/**
\predindex{generate\_clauses/4}
\predindex{between/4}
\predindex{generate\_clauses/6}
\predindex{gen\_off\_clauses/4}
\predindex{gen\_on\_clause/5}
\predindex{new\_offset/3}
\predindex{gen\_on\_final\_clause/7}
**/

gen_off_clauses([], _) --> !, [].
gen_off_clauses([C|Cs], CurSt, [(Head :- Goal)|Out], Tail) :-
  !,
  Head =..[CurSt, C, _, [], _, prev(PrevR,T,Goal), T, PrevR],
  gen_off_clauses(Cs, CurSt, Out, Tail).
gen_off_clauses(C, CurSt, [(Head :- Goal)|Tail], Tail) :-
  Head =..[CurSt, C, _, [], _, prev(PrevR,T,Goal), T, PrevR].

gen_on_clauses([], _, _) --> !, [].
gen_on_clauses([C|Cs], CurSt, NxtSt) -->
     !,
     gen_on_clause(C, CurSt, NxtSt),
     gen_on_clauses(Cs, CurSt, NxtSt).
gen_on_clauses(C, CurSt, NxtSt) -->
     gen_on_clause(C, CurSt, NxtSt).

gen_on_clause(C, CurSt, NxtSt, [(Head :- Body)| Tl], Tl) :-
   Head =..[CurSt,C,[C2|C2s],[C|Xs],P,PrevAccpSt,Term,R],
   Body =..[NxtSt,C2,C2s,Xs,P,PrevAccpSt,Term,R].

/**
\predindex{gen\_off\_clauses/4}
\predindex{gen\_on\_clause/5}
This predicate {\tt gen\_on\_final\_clause/7}, performs the following tasks
\begin{enumerate}
\item To ensure that unbound variables that are common in the Goal and the
Token are maintained, a {\tt copy\_term/2} is performed.
\item The predicate {\tt sub\_builtins/5} is invoked
to  run the  goal {\tt CopyGoal} and the input character list
{\tt C0s} through a simple processor that searches for commands
in the goal to perform tasks on the input character list.
It returns the remainder of the character list, a new goal
predicate, and a flag {\tt TextVariable} that indicates whether
the variable {\tt Text} (denoting the recognized input character list)
was accessed in the goal or not.
\item The final clause is now generated.
This clause had two possible heads. If the input character
list is now empty then a call to the {\tt NxtSt} is not needed.
Instead  the recognized  token is returned,
the lexeme -the recognized character list is returned as the token, and the
new goal, {\tt NewG}, is made the body. 
These tasks are exactly similar to what is done in {\tt gen\_off\_clauses/4}.

If the input character list is not empty
then a clause for {\tt NxtSt}  is generated, and
the current {\tt prev/3} term is replaced by the new {\tt prev/3} term.
In addition, if the {\tt TextVariable} flag was set during 
{\tt sub\_builtins/6}, then a copy of the recognized  character list {\tt Text}
is made so that the goal can operate on this copy while the other copy
can be used to search for potential longer tokens. 
\end{enumerate}

There are 3 execution times that one must be aware of here.
\begin{enumerate}
\item   {\bf Code generation time}, when PLEX
is generating code for the lexical analyzer,
That is when {\tt gen\_on\_final\_clause/7} will be executed.
\item  {\bf  Run-time, goal-time}, when the lexical analyzer
has recognized a token and is executing the goal corresponding to
that token. This is done only after the token has been completely
recognized. The clauses in {\tt DoGoal} here, and the code
created by {\tt gen\_off\_clauses/4} is executed at this time.
\item {\bf  Run-time but not goal time}, when the generated lexical analyzer
is executing, but before it has  recognized the token.
The code below that tests if the input character
list is empty {\tt C0s == []} and the {\tt strcpy/2} predicate are executed
here, along with calls to next state transition clauses.
\end{enumerate}
**/
gen_on_final_clauses([], _, _, _, _) --> !, [].
gen_on_final_clauses([C|Cs], CurSt, NxtSt, Tok, Goal) --> !,
  gen_on_final_clause(C, CurSt, NxtSt, Tok, Goal),
  gen_on_final_clauses(Cs, CurSt, NxtSt, Tok, Goal).
gen_on_final_clauses(C, CurSt, NxtSt, Tok, Goal) -->
  gen_on_final_clause(C, CurSt, NxtSt, Tok, Goal).

gen_on_final_clause(C, CurSt, NxtSt, Tok, Goal, 
[(Head :- (C0s == [] -> DoGoal;  C0s = [C2|C2s], Body)) |Tl], Tl) :-
     copy_term(Goal:Tok, CopyGoal:CopyT),
     sub_builtins(CopyGoal, C0s, Cs, CText, NewG, TextVariable),
     Head =..[CurSt,C,C0s,[C|Xs],Text,_,CopyT1,R],
     (var(TextVariable) ->
           Body =  NextState
      ; Body =   (plex:strcpy(Text,CText), NextState)
     ),
      NextState =.. [NxtSt,C2,C2s,Xs,Text,
             prev(Cs,CopyT,NewG),CopyT1,R],
      DoGoal = (Xs = [],  Text = CText, R = Cs, CopyT = CopyT1,  NewG).

/**
\predindex{gen\_on\_final\_clause/7}
\predindex{sub\_builtins/6}
The predicate below processes the goal of a PLEX rule, looking for
commands that indicate processing of the input character list in various ways.
The first 3 clauses handle compound goal forms. 
**/

sub_builtins((Cond->A;B), Ch0, Ch, Text, (Cond->NA,Ch=Ch1;NB,Ch=Ch2),TxtVar) :-
      !,
      sub_builtins(A, Ch0, Ch1, Text, NA, TxtVar),
      sub_builtins(B, Ch0, Ch2,  Text, NB, TxtVar).


sub_builtins((A,B), Ch0, Ch, Text, (NA,NB), TxtVar) :-
      !,
      sub_builtins(A, Ch0, Ch1, Text, NA, TxtVar),
      sub_builtins(B, Ch1, Ch,  Text, NB, TxtVar).

sub_builtins((A;B), Ch0, Ch, Text, (NA;NB), TxtVar) :-
      !,
      sub_builtins(A, Ch0, Ch1, Text, NA, TxtVar),
      sub_builtins(B, Ch1, Ch,  Text, NB, TxtVar).
/**
\predindex{sub\_builtins/6}
This predicate
finds a command {\tt insert/1} in the goal list to prefix a list of characters
to the input character list. The {\tt append/3} that does this
is generated and returned in the new version of the goal.
**/
sub_builtins(insert(Prefix), Ch0, Ch, _, append(Prefix, Ch0, Ch), _) :- !.

/**
\predindex{sub\_builtins/6}
The next two clauses handle commands that require portions of
the input character list to be ignored, e.g. in comments.
**/

% Ignore characters up until Pattern
sub_builtins(scan_past(Pat), Ch0, Ch, _,plex:scan_past(Pat,Ch0,Ch), _) :-  !.

% Collect string up until Pattern
sub_builtins(scan_string(Pat,String), Ch0, Ch, _,
    plex:scan_string(Pat,String,Ch0,Ch), _):-!.
/**
\predindex{sub\_builtins/6}
These next, built-ins must be mapped to the {\tt plex} module,
so that they are visible from wherever the user decided to
put this code.
**/
sub_builtins(float_chars(A,B),Ch,Ch, _, plex:float_chars(A,B),_):- !.
sub_builtins(strcpy(A,B),     Ch,Ch, _, plex:strcpy(A,B), _)    :- !.
sub_builtins(my_name(A,B),    Ch,Ch, _, plex:my_name(A,B), _)   :- !.
sub_builtins(dcg_append(A,B), Ch,Ch, _, plex:dcg_append(A,B), _):- !.

/**
\predindex{sub\_builtins/6}
Recursively call the current lexical analyzer
note that an empty character list is returned
to this level of lexical analysis, indicating that
no more tokens will be handled by this level
**/

sub_builtins(continue(Lexer,Tok), Ch0, [], _, Lex, _) :- !,
         Lex =..[Lexer, Ch0, Tok].
sub_builtins(plexmore(Lexer,Lexeme), [C|Ch0], Ch, _, Lex, _) :- !,
         Lex =..[Lexer, C, Ch0, Lexeme, Lexeme, _, _, Ch].


% Permanent switch to alternate lexical analyzer
sub_builtins(switch(Lexer,Tok), Ch0, [], _, Lex, _) :- !,
         Lex =.. [Lexer, Ch0, Tok].


% Switch to alternate lexical analyzer for one Token
%
sub_builtins(one(Lexer,Tok), Ch0, Ch, _, Lex, _) :- !,
         Lex =.. [Lexer, Ch0, Tok, Ch].

/**
\predindex{sub\_builtins/6}
This goal asks for the recognized input character list.
So the variable {\tt T} (corresponding to this) is 
unified with the variable in the goal. Also {\tt textmodified},
a flag is returned to indicate that the goal wished to access
the recognized input character list.
**/
sub_builtins(text(T), Ch, Ch, T, true, textmodified) :- !.

% Default catch-all clause that returns unrecognized goal commands as is
sub_builtins(Goal, Ch, Ch, _,  Goal, _).


/**
\predindex{sub\_builtins/6}
\section{Experiments}
This section studies  some performance issues in two different implementations
of PLEX {\tt plexV1} and {\tt plexV0},
 and compares them with the UNIX {\tt lex} and the GNU {\tt flex}, two
of the more popular lexical analyzer generator programs.

The objectives were to study and compare the power and expressiveness of
PLEX along with  time, and space requirements,  for lexical analyzer generation
and  lexical analysis. All the experiments were run on a DEC Station 5000/120,
with 16MBytes of main memory.

The two versions of PLEX differed in the techniques used to build the
lexical analyzer. In {\tt plexV1} equivalence classes were used to
group character symbols, and some efficient data structure and manipulation
routines for the follow position set and the symbol position set were
added. These were absent in {\tt plexV0}.

\subsection{Experiment 1}
The first experiment studied the performance of  PLEX and
{\tt lex} and {\tt flex}.  Since {\tt plexV0} and {\tt plexV1} both generated
identical lexical analyzers we used only one of them to do the
comparison.

We used an input file {\tt syn.spec} that described the
lexical specification for the SYNOPSYS language. The file was 34 lines long.
The generated lexical analyzers were tested on SYNOPSYS data files,
of varying length. These files were  duplicate copies of an original
file called {\tt sample.syn}, (107 lines long).
The table below summarizes the experiment. The time in
seconds is the run-time for the  lexical analyzers
to recognize tokens in the data file. 
 The comparisions may  not be very relevant, even though 
{\tt /usr/bin/time} was used in all 3 cases. 
{\tt plexV1} was timed as follows:
\verb@ echo "test('small.syn')." | /usr/bin/time ./synplex@
Only an approximate comparison is possible. Interestingly though,
despite the discrepancies in comparision,  
as the data files get larger the magnitude of difference in run-time
between {\tt plexV1} and {\tt flex/lex} seems to fall.
[Times were also generated via {\tt statistics/0}
for {\tt plexV1}, but they are
not shown here.] 
The lexical analyzer generated by {\tt plexV1}
was {\tt 4179} lines long, and had
33 states and 477 state table entries.
The {\tt lex} and {\tt flex} utilities
generated binary files so size comparision is irrelevant here.
From the -v flag settings we obtained the following 
information about the states and state transitions.
{\tt Flex} generated 46 states and  1232 table entries.
{\tt lex} generated 41 states, 935 transitions and 300 "packed"
transitions.
[NOTE: {\tt flex} was used with the -Cfe option
that says use equivalence classes for characters
and no table compression].
\begin{center}
\begin{tabular}{|r|r|c|c|c|} \hline
Data File&Length(lines)&lex(secs)&flex(secs)&plexV1(secs) \\ \hline
small.syn  &15     &0.3  &0.1 &5.7 \\
sample.syn &107    &0.3  &0.2 &5.7 \\
big.syn    &5,136  &9.6  &8.9 &23.4 \\
huge.syn   &10,272 &19.1 &17.8&45.1 \\
massive.syn&15,408 &29.2 &26.9&62.4 \\ \hline
\end{tabular}
\end{center}

\subsection{Experiment 2}
The second experiment that we performed was 
comparison of two input specifications, one more detailed
than the other with separate individual rules to identify all the keywords,
with a final  rule to catch identifiers. 
These specification were compared in terms of the 
the size and times of the generated lexical analyzers.

In this subsection we tabulate the results for building
{\tt plexV1} and {\tt plexV0} and compare the time taken to
build the DFA, after  the follow position set is generated, since this is the
place where we expect the maximum improvement in build time.
We also tabulate the number of positions generated when
equivalence classes are used for character symbols ({\tt plexV1}) and when
they are not ({\tt plexV0}).
 Two input specification files {\tt abc1.spec, abc4.spec}.
Both these files were identical to
the original input file (on which Experiment 1 was performed),
{\tt syn.spec} in all respects except for the keyword recognizing
rules. These rules  were absent in {\tt syn.spec}, which had 34 lines.
 The first file,
{\tt abc1.spec} contained half the   keyword list(93 lines), while the
second {\tt abc4.spec} contained roughly  1/3rd of the keyword rules 
(59 lines). The results are shown below.

\begin{center}
\begin{tabular}{|c|r|c|c|c|c|} \hline
File&Lexer Size&plexV0&plexV0&plexV1&plexV1 \\
 &(clauses) &Positions&Time&Positions&Time \\ \hline
syn.spec &4179  &462 &7.33s&6&2.1s \\
abc4.spec&37,075&1133&729.93s&430&202.6s \\
abc1.spec&67,155&1875&2908.92s&837&915.48s \\ \hline
\end{tabular}
\end{center}

This shows that grouping characters into equivalence classes is a big win
since it cuts down the state table construction time by at least a factor
of 4. But there are two other bottle necks now.
The first is the clause generation phase, which
converts the state table entries into clauses and writes them out.
This takes about 10-15 minutes for {\tt abc4.spec} and
about 45 minutes for {\tt abc1.spec}. 
The second bottleneck is the compilation of the generated lexical analyzer.
This takes about 30 minutes for {\tt abc4.spec} and over an
hour for {\tt abc1.spec}.
**/
/**
\section{Conclusions and Future Work}
We have implemented a lexical analyzer generator in
Prolog, {\tt plex}, that accepts an input regular expression specification
file, similar to that of the UNIX {\tt lex}. {\tt plex} is currently
capable of handling all the primitive regular expression operators
and most of the non-primitive operators: {\tt [],-, ?, +, ., \$}, and
\verb@\, ^@.   Other lex operators such as {\tt \{, \}, /}, need to be handled.


PLEX can take arbitrary Prolog predicates
in the goal following a token recognition. These
can be customized by the user to
manipulate the input character list.
For instance,  the {\tt scan\_past/2} construct in the goal
permits characters in the input character list to be skipped,
which might be useful while processing comments. Similarly 
{\tt insert/2} and {\tt plexmore/2} modify the character list;
the latter behaving like {\tt yymore()}.
{\tt lex} handles context-sensitive information by 
the \verb@<STATE>@ construct.  This is implemented in PLEX
in a very clean fashion. When a context-sensitive string
needs to be reecofnized, PLEX uses a {\tt switch/3} pedicate to
switch between lexicons. It can switch back and forth via
multual recursion.

In terms of efficiency, {\tt plex} is  not as fast {\tt lex} or {\tt flex}.
It can build DFAs with 500 states  and approximately 35K transitions
in about 10-15 minutes. Some improvements here are possible.
Currently we use equivalence classes to combine several character symbols
whose state behavior is similar. But we do not pack together
transitions that are similar in the state table. This could be a big savings
in space and time.   We have not used a profiler to track down the
 expensive predicates, and this may provide some savings.

We see two other versions of PLEX which may cause possible improvement
in build and/or run-time.
In the  first version,
we would replace the multiple off-clauses that are generated for
successive character symbols, by one huge off-clause, that would
take care of this by using arithmetic comparisons on a range.
Thus PLEX would still be deterministic but it would be slower. This is because
in the worst-case every branch in the arithmetic comparison
would be compared (like a CASE statement).
The second approach is to replace PLEX by a non-deterministic version
that backtracks to generate previous final states when it fails on
a transition. 
The tradeoffs between space and time efficiency here are not very clear
So we cannot say if they will definitely
be better or worse than the current version of PLEX.

The other bottleneck which is not a DFA construction issue, is that
{\tt plex} takes a long time to generate and write out the clauses that make 
up the lexical analyzer, from the state table. Moreover, since we make the DFA
deterministic, indexing on the character symbol for every state,
the DFA gets very large (see above) for state tables with several
transitions. This results in 
long compilation times to compile  the Prolog lexical analyzer into {\tt .qof}
format. 
\code{
\section{Some  Utility Predicates}
**/
new_offset(In, N, Set) :-
    integer(N),
    !,
    new_offset1(In, [N], Set).
new_offset(A, B, C) :-
    new_offset1(A, B, C).

new_offset1([],   _, []) :- !.
new_offset1(Off, [], Off) :- !.
new_offset1([C1|Os], [C2|Cs], Offset) :-
    !,
    ( C1 =:= C2 -> new_offset1(Os, Cs, Offset)
    ;  C1 < C2  -> Offset = [C1|Offset1],
                   new_offset1(Os, [C2|Cs], Offset1)
    ; new_offset1(Os, Cs, Offset)
    ).

between(A, Z, [A|List], Tail) :-
    ( A =:= Z -> List = Tail
    ; ( A < Z -> B is A + 1
      ;          B is A - 1
      ),
      between(B, Z, List, Tail)
    ).
/**
\predindex{new\_offset/3}
\predindex{between/4}
**/
digit(0'0).
digit(0'1).
digit(0'2).
digit(0'3).
digit(0'4).
digit(0'5).
digit(0'6).
digit(0'7).
digit(0'8).
digit(0'9).

/**
**/
my_contains_term(Sub,Term) :-
     nonvar(Term),
     ( functor(Sub,F,N),
       functor(Term,F,N) -> true
     ; Term =.. [_|List],
       mylist_contains_term(Sub, List)
     ).
/**
\predindex{my\_contains\_term/2}
\predindex{mylist\_contains\_term/2}
**/
   
mylist_contains_term(Sub, [Term|Ts]) :-
     ( my_contains_term(Sub, Term) -> true
     ; mylist_contains_term(Sub, Ts)
     ).
/**
\predindex{mylist\_contains\_term/2}
\predindex{my\_contains\_term/2}
}\endcode
\code{
**/
reverse(A,B) :- rev(A, [], B).

rev([],T,T).
rev([H|T],In,Out)   :-
   rev(T,[H|In],Out).
append_difflist(Afr,Unify,Unify,Bbk,Afr,Bbk).
difflist_to_list(L,[],L).
memberchk_dl(_, Fr,Bk) :- Fr == Bk,!,fail.
memberchk_dl(X, [X|_],_) :- !.
memberchk_dl(X, [_|Fr],Bk) :-
        memberchk_dl(X, Fr,Bk).

/**
\predindex{reverse/2}
\predindex{rev/3}
\predindex{memberchk\_dl/3}
Some debugging predicates
**/
c:-
 retractall(pos(_)),
 retractall(language_loaded(_)),
 retractall(language(_)).

write_list([]).
write_list([table(C,S,N,TA)|Tail]) :-
  !,
  format(user_error,"~p     ~p     ~p     ~p~n",[C,S,N,TA]),
  write_list(Tail).
write_list([Id-List|Tail]) :-
  !,
  format(user_error,"~p ",[Id]),
  format(user_error,"~p~n ",[List]),
  write_list(Tail).
write_list([X|Tail]) :-
  format(user_error,"~p~n ",[X]),
  write_list(Tail).
  
print_length_of_longest_PosSet(Sym_Pos_Set) :-
     ctr_is(0,Pos),
     format(user_error,"Total Number of Positions ~d~n",Pos),
     keys_and_values(Sym_Pos_Set,_,PosSet),
     get_lengths(PosSet,LenSet),
     sort(LenSet,Sorted_LenSet),
     last(LongestLen,Sorted_LenSet),
     format(user_error,"Largest # of posns",[]),
     format(user_error," assoc. with a sym ~d~n",LongestLen).

get_lengths([],[]).
get_lengths([H|Tl],[LenH|LenTl]) :-
    length(H,LenH),
    get_lengths(Tl,LenTl).
/**
\predindex{c/0}
\predindex{write\_list/1}
\predindex{print\_length\_of\_longest\_PosSet/1}
\predindex{ctr\_is/2}
\predindex{keys\_and\_values/3}
\predindex{get\_lengths/2}
\predindex{last/2}
\appendix
  Here is an example of a very simple plex spec file and the
lexical analyzer code it generates.
\begin{verbatim}
% test.spec 
test lexicon

"ab" is  ab;
"." is  x.
\end{verbatim}

The lexical analyzer DFA has 4 states, which are named by predicates
\verb@ test, test_1, test_2,@ and \verb@ test_3@ respectively.
Since a DFA  represents transitions 
from a given state on all possible symbols, a majority of
the predicates for  each state correspond to the default transition
indicating what happens when the symbol at a state is not an {\tt a(98)}
or a {\tt b(99)}.
**/

write_clauses([],_).
write_clauses([C|Cs],Stream) :-
    format(Stream,'~q.~n',[C]),
    write_clauses(Cs,Stream).

vformat(Stream, Format, Data) :-
     verbose -> 
          append("PLEX: ",Format,LFormat),
          format(Stream, LFormat, Data)
     ; true.

/**
\predindex{write\_clauses/2}
\predindex{vformat/3}
\predindex{verbose/0}
Built-ins (formerly in plex_runtime).
scan_string/4, float_chars/2, scan_past/3, strcpy/2, my_name, dcg_append/3.
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
}\endcode
\code{
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
**/
strcpy(V,[]) :- var(V),!.
strcpy([H|T],[H|NT]) :- strcpy(T,NT).
/**
\predindex{strcpy/2}
**/
remove_det(H,[H|T],    T) :- !.
remove_det(A,[H|T],[H|R]) :-
      remove_det(A,T,R).

my_name('IsVaR',[10|Error]) :- !,
    append("<<<ERROR>>>",[10],Error).
my_name(number(F),Chars)   :-
    !,
    (float(F) -> my_name(float(F),Chars)
    ;            my_name(integer(F),Chars)
    ).

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
my_name(N,[32|Chars])          :- name(N,Chars).

quote_type(single(Cs),0'',Cs).
quote_type(double(Cs),0'",Cs).

dcg_append(A,B,C) :- append(A,C,B).

/**
\predindex{remove\_det/3}
\predindex{my\_name/2}
\predindex{quote\_type/3}
\predindex{dcg\_append/3}
}\endcode
**/

keysort_union(List, Sorted) :-
    keysort(List, List1),
    combine(List1, Sorted).

combine([], []).
combine([Key-Val0|List1], Sorted) :-
    combine(List1, Sorted, Key, Val0).

combine([K-Val1|List1], Sorted, Key, Val0) :-
    K == Key,
    !, 
    ord_union(Val0, Val1, Val2),
    combine(List1, Sorted, Key, Val2).
combine(List1, [Key-Val0|Sorted], Key, Val0) :-
    combine(List1, Sorted).


