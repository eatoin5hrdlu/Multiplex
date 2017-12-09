/**
% VHDL FORMAT FILE
%
% Copyright 1991 by INTERGRAPH 
% Author:  Peter B. Reintjes

\chapter{VHDL: A Grammar for MULTIPLEX}
\section{VHDL Lexical Analyzer}
**/
vhdl lexicon

"[ \t\n]+" is  [];
"\."    is    '.' ;  % period
";"     is    ';' ;  % semicolon
"'"     is    '''';  % single quote
","     is    ',' ;  % comma
":"     is    ':' ;  % colon
"&"     is    '&' ; 
"\|"     is    '|' ; 
"<="     is    '<=' ; 
"<"     is    '<' ; 
">="     is    '>=' ; 
">"     is    '>' ; 
":="     is    ':=' ; 
"="     is    '=' ; 
"/="     is    '/=' ; 
"!"     is    '!' ;
"\*"    is    '*' ;
"\*\*"    is    '**' ;
"\+"    is    '+' ;
"-"    is    '-' ;
"\("    is    '(' ;
"\)"    is    ')' ;
"\{"     is    '{' ;
"\}"     is    '}' ;

'"[^"]*"' is  quote(double(Chs)) if  (name(Chs,T), text(T)); 


"\-\-"    is    Result if  (scan_past("\n"), continue(vhdl,Result));

"[+-]?[0-9]+"                           is integer(N) if (name(N,T),text(T));
"[+-]?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?" is float(F)   
   if (float_chars(F,T), text(T));
"-\.[0-9]+([eE][+-]?[0-9]+)?"           is float(F)   
    if (float_chars(F,T),text(T));
"\.[0-9]+([eE][+-]?[0-9]+)?"            is float(F) 
     if (float_chars(F,T),text(T));
"[a-zA-Z_][a-zA-Z0-9<>_]*"              is [Tok|Ts] 
      if (name(N,T),text(T),
          (keyword(N) ->
              Tok=N,
              switch(vhdl,Ts)
           ; Tok = id(N),
             switch(vhdl,Ts)
          )).

keyword(abs).
keyword(access).
keyword(after).
keyword(alias).
keyword(all).
keyword(and).
keyword(architecture).
keyword(array).
keyword(assert).
keyword(attribute).
keyword(begin).
keyword(block).
keyword(body).
keyword(buffer).
keyword(bus).
keyword(case).
keyword(component).
keyword(configuration).
keyword(constant).
keyword(disconnect).
keyword(downto).
keyword(else).
keyword(elsif).
keyword(end).
keyword(entity).
keyword(exit).
keyword(file).
keyword(for).
keyword(function).
keyword(generate).
keyword(generic).
keyword(guarded).
keyword(if).
keyword(in).
keyword(inout).
keyword(is).
keyword(label).
keyword(library).
keyword(linkage).
keyword(loop).
keyword(map).
keyword(mod).
keyword(nand).
keyword(new).
keyword(next).
keyword(nor).
keyword(not).
keyword(null).
keyword(of).
keyword(on).
keyword(open).
keyword(or).
keyword(others).
keyword(out).
keyword(package).
keyword(port).
keyword(procedure).
keyword(process).
keyword(range).
keyword(record).
keyword(register).
keyword(rem).
keyword(report).
keyword(return).
keyword(select).
keyword(severity).
keyword(signal).
keyword(subtype).
keyword(then).
keyword(to).
keyword(trace).
keyword(transport).
keyword(type).
keyword(units).
keyword(until).
keyword(use).
keyword(variable).
keyword(wait).
keyword(when).
keyword(while).
keyword(with).
keyword(xor).

/**
\section{The VHDL Grammar}

This is the MULTIPLEX Grammar for VHDL.
It is derived from an early implementation of
a VHDL parser developed jointly under contract with
IBM Corporation and the Microelectronics Center of North
Carolina, by the same author. The public-domain version
is still available via anonymous FTP from {\tt mcnc.org}.

Coincidentally, the entire parser for IEEE 1076 standard of VHDL
was implemented in 1076 lines of Prolog, thus making it between
one tenth and one fifth the size of other VHDL implementations.
This demonstrates vividly that complex VLSI
systems will not require millions of lines of code if they
are described at an appropriate level of abstraction.

This parser can become the basis of a wide variety of tools which
require input in the VHDL language. In particular, this parser
was developed in conjunction with software components for
schematic entry, logic synthesis, and a universal hardware
description language translator.
This version of the parser makes no semantic consistency
checks on the VHDL that it reads, and only requires that the VHDL
be syntactically correct.

This parser is based up the grammar in
the appendix of {\it VHDL: Hardware Description and Design},
(Kluwer Academic Press, 1989).
According to the authors, this grammar is simpler than the
official definition in the IEEE Standard, but is functionally
equivalent.  The DCG rules in this report are numbered to
coincide with the grammar rules in the book.
Thus, this implementation may be useful
to VLSI/CAD developers who are not using Prolog because
this implementation of the VHDL grammar
contains corrections and clarifications that other
textual specifications have omitted.
For example, a direct implementation of the grammar
given in {\it VHDL: Hardware Description and Design},
(Kluwer Academic Press, 1989) would contain:

\begin{itemize}

\item{}
Left-recursive rules which would require special code to guard
against infinite recursion, and even then could increase the
running time exponentially.

\item{}
Overlapping rules, requiring arbitrarily
large look-ahead buffering.

\item{}
Incorrect grammar rules.

\end{itemize}

Since some readers of a grammar specification will be 
charged with the task of implementing a parser,
it would be nice if they could trust the specification.
In this parser, all left-recursive rules have been translated
into right-recursions and overlapping rules have been combined
to eliminate look-ahead.
Furthermore, this report describes the code to handle the
correct parsing of operators, something completely missing from
the textual specification above.
But most importantly, this ``specification''
has succesfully parsed hundreds of VHDL files,
an exercise which flushed out errors in the grammar rules.
**/
vhdl ::= file('name.vhdl', vhdl), vhdl_design_units.

vhdl_design_units ::= vhdl_du, vhdl_design_units.
vhdl_design_units ::= [].

vhdl_du  ::= vhdl_design_unit.

/**
\subsection{Literals and Miscellaneous}
The grammar rules first define such things as identifiers,
constant values, and expressions involving monadic (e.g. NOT)
and diadic (e.g. AND) operators. The values and
expressions recognized here are used throughout
the remaining grammar rules.
\subsubsection{Rule 1}
**/
vhdl_designator(ID) ::= vhdl_identifier(ID), !.
vhdl_designator(ST) ::= [string(SL)], {name(ST,SL)}.
/**
\subsubsection{Rule 2}
**/
vhdl_literal(null) ::= [null],!.
% vhdl_literal(L)  ::= [ bit_string(L) ], !.
vhdl_literal(L)    ::= [ string(L) ], !.
vhdl_literal(L)    ::= vhdl_abstract_literal(L).
vhdl_literal(L)    ::= vhdl_enumeration_literal(L).
vhdl_literal(L)    ::= vhdl_physical_literal(L).
/**
\subsubsection{Rule 3}
We only have one kind of number at present which includes
integers and floating point numbers.
**/
vhdl_abstract_literal(F) ::= [ number(F) ].
/**
\subsubsection{Rule 4}
**/
vhdl_enumeration_literal(char(L))  ::= [ char(L) ].
vhdl_enumeration_literal(ID) ::= vhdl_identifier(ID).
/**
\subsubsection{Rule 5}
**/
vhdl_physical_literal(pl(AL,ID)) ::=
        vhdl_abstract_literal(AL), vhdl_identifier(ID).
/**
\subsubsection{Rule 6}
An identifier must not be a keyword-token.
**/
%vhdl_identifier(ID) ::= [id(ID)].
vhdl_identifier(ID) ::= [ID], user(functor(ID,id,_)).

%
% The first 2 arguments represent a difference list, the
% third is the same identifier list in the regular form
%
vhdl_identifier_list([ID|IDs0], IDs1, [ID|IDs]) ::=
        vhdl_identifier(ID), [','],
        vhdl_identifier_list(IDs0, IDs1, IDs).
vhdl_identifier_list([ID|IDs], IDs, [ID]) ::=
        vhdl_identifier(ID).

      
/**
 
 
\subsection{Design Unit}
The Design Unit is the top-level construction of the parse
tree, this is the data structure
containing the information from an entire VHDL file.

\subsubsection{Rule 7}
**/
vhdl_design_unit ::=
        vhdl_opt_context_items,
        vhdl_library_unit.
/**
 
 
\subsubsection{Rule 8}
**/
vhdl_library_unit  ::= vhdl_entity_declaration.
%vhdl_library_unit ::= vhdl_configuration_declaration.
%vhdl_library_unit ::= vhdl_package_declaration.
vhdl_library_unit  ::= vhdl_architecture_body.
%vhdl_library_unit ::=  vhdl_package_body.
/**
\subsubsection{Rule 9}
**/
vhdl_opt_context_items ::=
        vhdl_library_clause,
        vhdl_opt_context_items.
vhdl_opt_context_items ::= [].
/**
\subsubsection{Rule 10}
**/
vhdl_library_clause ::=
       [ library ], vhdl_identifier_list(_).
/**
\subsubsection{Rule 11}
**/
vhdl_use_clause ::=
       [ use ], vhdl_selected_name,
                vhdl_selected_names.

vhdl_selected_names ::=
       [ ',' ],  vhdl_selected_name,
                   vhdl_selected_names.

vhdl_selected_names ::= [].
/**
\subsection{Library Units}
\subsubsection{Rule 12}

The grammar for Rule 12 is incorrect as given 
in the appendix of {\it VHDL: Hardware Design and Description}.
The discussion of the entity declaration on page 31
correctly (but informally) gives the correct form, 
but the grammar rule in the appendix leaves out the
optional declarative items.
**/
vhdl_entity_declaration ::= 
       [ entity ],  vhdl_identifier(ID), [ is ],
	update(module_name(ID)),
         vhdl_opt_generic_statement,
         vhdl_opt_port_statement,
         vhdl_opt_declarative_items,
         vhdl_opt_entity_body,       
       [ end ], vhdl_opt_identifier(ID), [';'].

/**
**/
vhdl_opt_entity_body ::=
         [ begin ], vhdl_concurrent_statements.
vhdl_opt_entity_body ::= [].

vhdl_opt_generic_statement ::=
        [ generic ],  vhdl_interface_list.
          
vhdl_opt_generic_statement ::= [].
/**
**/
vhdl_opt_port_statement ::=
        [ port ],  vhdl_interface_list.

vhdl_opt_port_statement ::= [].

/**
\subsubsection{Rule 13}
**/
vhdl_architecture_body ::= 
       [ architecture ],  vhdl_identifier(Id),
       [ of ], vhdl_mark(_Entity),  [ is ], 
       vhdl_opt_declarative_items,
       [ begin ],
       vhdl_concurrent_statements,
       [ end ], 
       vhdl_opt_identifier(_), [';'].
/**
\subsubsection{Rule 14}
**/
vhdl_configuration_declaration ::=
       [ configuration ], vhdl_identifier(ID),
           [ of ], vhdl_mark(Entity), [ is ],
              vhdl_opt_declarative_items(DIs),
         vhdl_block_configuration(Block),
       [ end ], vhdl_opt_identifier(ID).
/**
\subsubsection{Rule 15}
**/
vhdl_package_declaration ::=
       [ package ], vhdl_opt_identifier(ID), [ is ],
         vhdl_opt_declarative_items(Declarations),
       [ end ], vhdl_opt_identifier(ID).

/**
\subsubsection{Rule 16}
**/
vhdl_package_body ::=
       [ package,  body ], vhdl_identifier(ID), [ is ],
         vhdl_opt_declarative_items(DIs),
       [ end ], vhdl_opt_identifier(ID).

vhdl_opt_identifier(ID)  ::= vhdl_identifier(ID).
vhdl_opt_identifier(_)::= [].

/**
The last part of this production, {\tt vhdl\_opt\_identifier//1},
is called with
its argument instantiated. This will succeed if the
identifier found in the input stream matches
the one recognized at the beginning of the rule,
but will also succeed if there is no identifier.
This is in contrast to the other optional rules which
instantiate their arguments to {\tt null}.
\subsection{Declarative Items}
\subsubsection{Rule 17}
**/
%vhdl_declarative_item ::= vhdl_type_declaration.
%vhdl_declarative_item ::= vhdl_subtype_declaration.
vhdl_declarative_item ::= vhdl_object_declaration.
%vhdl_declarative_item ::= vhdl_file_declaration.
%vhdl_declarative_item ::= vhdl_alias_declaration.
%vhdl_declarative_item ::= vhdl_subprogram_declaration_or_body(DI).
vhdl_declarative_item ::= vhdl_component_declaration.
%vhdl_declarative_item ::= vhdl_attribute_declaration(DI).
%vhdl_declarative_item ::= vhdl_attribute_specification(DI).
%vhdl_declarative_item ::= vhdl_configuration_specification(DI).
%vhdl_declarative_item ::= vhdl_disconnection_specification(DI).
%vhdl_declarative_item ::= vhdl_use_clause(DI).
/**
**/
vhdl_opt_declarative_items ::=
        vhdl_declarative_item,
        [';'],
        vhdl_opt_declarative_items.

vhdl_opt_declarative_items ::= [].
/**
\subsection{Subprograms}
The rules for subprogram declarations and subprogram
bodies have been combined because a declaration is
equivalent to the specification part of the program body.
If we read a subprogram specification and the next 
token is not the keyword {\bf is}, then we have just
read a subprogram declaration.
It would be impossible to backtrack out of the subprogram
declaration and try the subprogram body rule because
several token lines may have been read while getting the
interface-list part of the subprogram specification.
\subsubsection{Rule 18}
**/
vhdl_subprogram_declaration_or_body(sub_program(SS,Body)) ::=
        vhdl_subprogram_specification(Name,SS),
        vhdl_opt_subprogram_body(Name,Body).
/**
\subsubsection{Rule 19}
**/
vhdl_subprogram_specification(D,sub_spec(D,IL,null)) ::=
        [ procedure ], vhdl_designator(D),
        vhdl_opt_interface_list(IL).
vhdl_subprogram_specification(D,sub_spec(D,IL,TM)) ::=
        [ function ],  vhdl_extended_designator(D),
           vhdl_opt_interface_list(IL),
        [ return ], vhdl_mark(TM).
/**
The difference between a {\tt vhdl\_designator//1}
and a {\tt vhdl\_extended\_designator//2} is that we allow
keywords to be used as extended designators.
For example, the + operator is a legal function name
if we are overloading the plus operator.
**/
vhdl_extended_designator(ST,[string(SL)|T],T) :- !, name(ST,SL).
vhdl_extended_designator(ID,[ID|T],T).
/**
\subsubsection{Rule 20}
**/
vhdl_opt_subprogram_body(Name,program_body(Is,Ss)) ::=
        [ is ], !,
           vhdl_opt_declarative_items(Is),
        [ begin ], !,
           vhdl_sequential_statements(Ss),
        [ end ], vhdl_opt_designator(Name).

vhdl_opt_subprogram_body(_,null) ::= [].

vhdl_opt_designator(D) ::= vhdl_extended_designator(D),!.
vhdl_opt_designator(_) ::= [].
/**
\subsection{Interface Lists and Association Lists}
\subsubsection{Rule 21}
**/
vhdl_interface_list ::=
        ['('], vhdl_interface_elements(Ies, []),
        update(ports(Ies)).

vhdl_opt_interface_list ::= vhdl_interface_list.
vhdl_opt_interface_list ::= [].
/**
**/
vhdl_interface_elements(Ies0, Ies) ::=
        vhdl_interface_element(Ies0, Ies1),
        vhdl_ie_sub(Ies1, Ies).
 
vhdl_ie_sub(Ies, Ies) ::= [')'],[';'] .
vhdl_ie_sub(Ies0, Ies) ::= [';'], vhdl_interface_elements(Ies0, Ies).
/**
\subsubsection{Rule 22}
**/
vhdl_interface_element(IL0, IL) ::=
        vhdl_opt_object_class(Class),
%        vhdl_identifier_list(IL), [':'],
        vhdl_identifier_list(IL0, IL, IdList), [':'],
        vhdl_opt_mode(Mode),
        vhdl_subtype_indication(SubType),
        vhdl_opt_bus(_), vhdl_opt_assignment(Expr),
	update(mode(IdList, Mode)),
	updates(data(IdList, SubType, Expr)),
	update(object(IdList, Class)).

/**
\subsubsection{Rule 23}
**/
vhdl_opt_mode(input)      ::= [  in ].
vhdl_opt_mode(output)     ::= [  out ].
vhdl_opt_mode(bidir)   ::= [ inout ].
vhdl_opt_mode(buffer)  ::= [ buffer ].
vhdl_opt_mode(linkage) ::= [ linkage ].
vhdl_opt_mode(null) ::= [].
/**
**/
vhdl_opt_bus(bus) ::= [ bus ].
vhdl_opt_bus(null) ::= [].
/**
\subsubsection{Rule 24}
**/
vhdl_association_list([F|Fs],[A|As]) ::=
      ['('], vhdl_association_element(F,A),
             vhdl_more_association_list(Fs,As).
/**
**/
vhdl_more_association_list([],[])     ::= [')'],[';'].
vhdl_more_association_list([FE|FEs],[AE|AEs]) ::=
        [','], vhdl_association_element(FE,AE),
        vhdl_more_association_list(FEs,AEs).
/**
**/
vhdl_opt_association_list(Fs,As)  ::= vhdl_association_list(Fs,As).
vhdl_opt_association_list([],[]) ::= [].
/**
\subsubsection{Rule 25}
**/
vhdl_association_element(Formal, Actual) ::=
      vhdl_opt_formal_part(Formal), vhdl_actual_part(Actual).
/**
\subsubsection{Rule 26}
**/
vhdl_formal_part(Name) ::= vhdl_name(Name).
%vhdl_formal_part(formal(FM,Name)) ::=
%         vhdl_mark(FM), ['('], vhdl_name(Name), [')'].
/**
**/
vhdl_opt_formal_part(Name) ::= vhdl_formal_part(Name), ['=>'].
vhdl_opt_formal_part(null) ::= [].
/**
\subsubsection{Rule 27}
**/
vhdl_actual_part(open) ::= [ open ].
vhdl_actual_part(Expr) ::= expression(Expr).
/**
\subsection{Names and Expressions}
Since Multiplex provides a powerful expression 
parsing mechanism, we only need to define
the VHDL operators. The operator and primary-expression
definitions take the place of rule 29 in the VHDL grammar.

\subsubsection{Rule 28}
**/
vhdl_mark(_,[],_) :- !, fail.
/**
**/
vhdl_mark(ID) ::= vhdl_identifier(ID).
%vhdl_mark(SN) ::= vhdl_selected_name(SN).
/**
**/
vhdl_opt_mark(M)    ::= vhdl_mark(M).
vhdl_opt_mark(null) ::= [].
/**
\subsubsection{Rule 30}
**/
vhdl_primary(P) ::= ['('], expression(P), [')'].
vhdl_primary(P) ::= vhdl_aggregate(P).
vhdl_primary(P) ::= vhdl_function_call(P).
vhdl_primary(P) ::= vhdl_qualified_expression(P).
vhdl_primary(P) ::= vhdl_name(P).
vhdl_primary(P) ::= vhdl_literal(P).
vhdl_primary(P) ::= vhdl_type_conversion(P).
vhdl_primary(P) ::= vhdl_allocator(P).
/**
\subsubsection{Rule 31}
**/
%vhdl_name(N) ::= [id(N)],!.
vhdl_name(N) ::= [N], user(functor(N,id,_)).
%vhdl_name(attr_name([N,A|As])) ::=
%                 [N, attr(A)], !, vhdl_opt_attrs(As).
%vhdl_name(N) ::= vhdl_designator(N).
%vhdl_name(N) ::= vhdl_selected_name(N).
%vhdl_name(N) ::= vhdl_indexed_name(N).
%vhdl_name(N) ::= vhdl_slice_name(N).
/**
**/
vhdl_opt_attrs([A|As]) ::= [attr(A)], vhdl_opt_attrs(As).
vhdl_opt_attrs([])     ::= [].
/**
\subsubsection{Rule 32}
**/
vhdl_selected_name(vhdl_name(P,S)) ::=
       vhdl_prefix(P), ['.'], vhdl_suffix(S).
/**
**/
vhdl_prefix(_,  [],_) :- !,fail.
/**
\subsubsection{Rule 33}
The prefix definition given in {\it VHDL: Hardware Description and Design}
is problematic in that it is defined in terms of {\tt names} which are
in turn defined as {\tt selected\_names} or {\tt indexed\_names} each
of which can begin with a prefix.
Thus, the rules for {\tt vhdl\_prefix//1},
as given by the book would be:
\begin{verbatim}
vhdl_prefix(P) ::= vhdl_name(P).
vhdl_prefix(P) ::= vhdl_function_call(P).
\end{verbatim}
This, through {\tt vhdl\_name//1} and {\tt vhdl\_selected\_name//1}
results in a left-recursive definition
unsuitable for implementation in Prolog.
So we have define {\tt vhdl\_prefix//1} to be
any number of primitive prefixes composed of an
identifier and the punctuation of a
selected or indexed name, followed by a prefix.
This right-recursive production will recognize the same
compound prefixes as specified by the book.
**/
vhdl_prefix(prefix(Prefix)) ::=
        vhdl_designator(P1), vhdl_prefix(P1,Prefix).

vhdl_prefix(P1,function_call(P1,Args,P2)) ::=
        vhdl_association_list(Args), vhdl_prefix2(P2).
/**
**/
vhdl_prefix(P1,slice(P1,R,P2)) ::=
        [ '(' ], vhdl_discrete_range(R), [')'], vhdl_prefix2(P2).

vhdl_prefix(P1,select(P1,P2)) ::=
        [ '.' ], vhdl_prefix2(P2).
/**

**/
vhdl_prefix(P1,attribute(P1,Attr,E,P2)) ::=
        [ attr(Attr) ], vhdl_opt_paren_expression(E), vhdl_prefix2(P2).

vhdl_prefix(P1,P1) ::= []. % Just the designator

vhdl_prefix2(P) ::= vhdl_prefix(P).
vhdl_prefix2(null) ::= [].
/**
**/
vhdl_opt_paren_expression(E) ::=
         ['('],expression(E),[')'].
vhdl_opt_paren_expression(null) ::= [].
/**
\subsubsection{Rule 34}
**/
vhdl_suffix(all)       ::= [ all ].
vhdl_suffix(char(C))   ::= [ char(C) ].
vhdl_suffix(dot(S,S2)) ::= vhdl_designator(S), ['.'], vhdl_suffix(S2).
vhdl_suffix(S)         ::= vhdl_designator(S).
/**
 
\subsubsection{Rule 35}
**/
vhdl_indexed_name(vhdl_name(P,[E|Es],Attr)) ::=
       vhdl_prefix(P), ['('],
       expression(E),
       vhdl_expression_list(Es),
       vhdl_opt_attrs(Attr).
/**
 
**/
vhdl_expression_list([]) ::= [')'].

vhdl_expression_list([Expr|Exprs]) ::=
        [','], expression(Expr),
               vhdl_expression_list(Exprs).
/**
 
\subsubsection{Rule 36}
**/
vhdl_slice_name(vhdl_name(P,Range)) ::=
       vhdl_prefix(P),
       ['('], vhdl_discrete_range(Range), [')'].
/**
 
\subsubsection{Rule 37}
**/
vhdl_attribute_name(vhdl_name(P,Attr,Expr)) ::=
       vhdl_prefix(P), [ attr(Attr) ],
       vhdl_opt_static_expression(Expr).
/**
 
**/
vhdl_opt_static_expression(Expr) ::=
        ['('], expression(Expr), [')'].

vhdl_opt_static_expression(null) ::= [].
/**
 
\subsubsection{Rule 38}
**/
vhdl_function_call(vhdl_call(Call,Args)) ::=
       vhdl_mark(Call), vhdl_association_list(Args).
/**
 
\subsubsection{Rule 39}
**/
vhdl_aggregate([A|As]) ::= ['('],
        vhdl_element_association(A), vhdl_more_aggregate(As).

vhdl_more_aggregate([])     ::= [')'].
vhdl_more_aggregate([A|As]) ::=
        [','], vhdl_element_association(A),
        vhdl_more_aggregate(As).
/**
 
\subsubsection{Rule 40}
**/
vhdl_qualified_expression(qual_expr(Type,Expr)) ::=
       vhdl_mark(Type), [ attr('(') ],
       vhdl_expression_or_aggregate(Expr), [')'].
/**
 
**/
vhdl_expression_or_aggregate(Expr) ::= expression(Expr).

vhdl_expression_or_aggregate(Ag) ::= vhdl_aggregate(Ag).
/**
 
\subsubsection{Rule 41}
**/
vhdl_type_conversion(type_conversion(Type,Expr)) ::=
       vhdl_mark(Type), ['('], expression(Expr), [')'].
/**
 
 
\subsubsection{Rule 42}
**/
vhdl_allocator(A) ::= [new], vhdl_alloc_subterm(A).

vhdl_alloc_subterm(A) ::= vhdl_subtype_indication(A).
vhdl_alloc_subterm(A) ::= vhdl_qualified_expression(A).
/**
\subsection{Operators}

The built-in Multiplex expression parser requires the following 
operator definitions.
**/

operator(100, xfy,  and) ::= [and].
operator(100, xfy, nand) ::= [nand].
operator(100, xfy,   or) ::= [or].
operator(100, xfy,  nor) ::= [nor].
operator(100, xfy,  xor) ::= [xor].

/**

**/
operator(200, xfy,  equal)   ::= ['='].
operator(200, xfy,  less)    ::= ['<'].
operator(200, xfy,  greater) ::= ['>'].
operator(200, xfy,  lessequal)    ::= ['<='].
operator(200, xfy,  greaterequal) ::= ['>='].
operator(200, xfy,  notequal)     ::= ['/='].
/**

**/
operator(400, xfy,  plus)      ::= ['+'].
operator(400, xfy,  minus)     ::= ['-'].
operator(400, xfy,  ampersand) ::= ['&'].
/**

**/
operator(500, xfy,  times) ::= ['*'].
operator(500, xfy, divide) ::= ['/'].
operator(500, xfy, mod)    ::= [mod].
operator(500, xfy, rem)    ::= [rem].
operator(600, xfy, exp)    ::= ['**'].

/**
**/
operator(100, fx, abs)   ::= [abs].
operator(100, fx, not)   ::= [not].
operator(100, fx, plus)  ::= ['+'].
operator(100, fx, minus) ::= ['-'].

/**
**/
primary(Expr)       ::=  ['('], expression(Expr), [')'].
primary(float(F))   ::=  [ float(F) ].
primary(integer(F)) ::=  [ integer(F) ].
%primary(id(A))      ::=  [id(A)].
primary(A)      ::=  [A], user(functor(A,id,_)).

/**
**/
/**
\subsubsection{Rule 44}
**/

vhdl_unary_operator(OP,[OP|T],T) :- vhdl_unary_op(OP).

/**

\subsection{Element Association and Choices}

\subsubsection{Rule 45}
**/
vhdl_element_association(element_association(C,Expr)) ::=
       opt_vhdl_choices(C), expression(Expr).

opt_vhdl_choices(C)    ::= vhdl_choices(C), ['=>'].
opt_vhdl_choices(null) ::= [].
/**

\subsubsection{Rule 46}
**/
vhdl_choices([C|Cs]) ::=
        vhdl_choice(C), vhdl_more_choices(Cs).
/**

**/
vhdl_more_choices([C|Cs]) ::= ['|'],
       vhdl_choice(C),
       vhdl_more_choices(Cs).
vhdl_more_choices([]) ::= [].
/**

The rule for {\tt vhdl\_choices//1} is a good example of
how we can make the grammar more efficient by having
one rule for the first item in a series and a second rule
for the (possibly null) continuation of the series. 
Note that presence of the vertical bar determines
the rule for {\tt vhdl\_more\_choices//1}.  A more intuitive,
but much less efficient grammar might use the following rules:

\begin{verbatim}
slow_vhdl_choices([C|Cs]) ::=
        vhdl_choice(C), ['|'], slow_vhdl_choices(Cs).

slow_vhdl_choices([C]) ::=
        vhdl_choice(C).
\end{verbatim}

The problem with this is that {\tt vhdl\_choice//1} can
be an arbitrarily complex expression.  Every time {\tt vhdl\_choices}
is used, there will be one expression which is completely parsed
twice, once before realizing that it is not followed by a vertical
bar and then once again because it is the last (or only) choice.

\subsubsection{Rule 47}
**/
vhdl_choice(others) ::= [others].
vhdl_choice(C)      ::= expression(C).
vhdl_choice(C)      ::= vhdl_discrete_range(C).
/**

\subsection{Type Declarations}
\subsubsection{Rule 48}
**/
vhdl_type_declaration(vhdl_type(ID,Definition)) ::=
         [type], vhdl_identifier(ID),
         [is], vhdl_type_definition(Definition).
/**

\subsubsection{Rule 49}
**/
vhdl_type_definition(Def) ::=
         vhdl_enumeration_type_definition(Def).
vhdl_type_definition(Range) ::=
         vhdl_range_constraint(Range).
vhdl_type_definition(Physical) ::=
         vhdl_physical_type_definition(Physical).
vhdl_type_definition(Array) ::=
         vhdl_unconstrained_array_definition(Array).
vhdl_type_definition(Array) ::=
         vhdl_constrained_array_definition(Array).
vhdl_type_definition(RType) ::=
         vhdl_record_type_definition(RType).
vhdl_type_definition(Access) ::=
         vhdl_access_type_definition(Access).
vhdl_type_definition(FileType) ::=
         vhdl_file_type_definition(FileType).
/**
 
\subsubsection{Rule 50}
**/
vhdl_enumeration_type_definition([E|Enums]) ::=
        ['('], vhdl_enumeration_literal(E),
        vhdl_enumeration_literal_list(Enums).

vhdl_enumeration_literal_list([])     ::= [')'].
vhdl_enumeration_literal_list([E|Es]) ::=
        [','], vhdl_enumeration_literal(E),
        vhdl_enumeration_literal_list(Es).
/**
 
\subsubsection{Rule 51}
**/
vhdl_physical_type_definition(vhdl_type(Range,Base,Secondary)) ::=
        vhdl_range_constraint(Range),
        [ units ], 
          vhdl_base_unit_declaration(Base),
          vhdl_secondary_unit_declarations(Secondary),
        [end, units].
/**
 
**/
vhdl_secondary_unit_declarations([S|Ss]) ::=
        vhdl_secondary_unit_declaration(S),
        vhdl_secondary_unit_declarations(Ss).

vhdl_secondary_unit_declarations([]) ::= [].
/**
 
\subsubsection{Rule 52}
**/
vhdl_base_unit_declaration(ID) ::= vhdl_identifier(ID).
/**
 
\subsubsection{Rule 53}
**/
vhdl_secondary_unit_declaration(vhdl_sec(ID,PL)) ::=
        vhdl_identifier(ID),
        ['='],
        vhdl_physical_literal(PL).
/**
 
\subsubsection{Rule 54}
**/
vhdl_unconstrained_array_definition(vhdl_array([ISD|ISDs],SI)) ::=
        [array, '('],
             vhdl_index_subtype_definition(ISD),
             vhdl_index_subtype_definitions(ISDs),
        [of], vhdl_subtype_indication(SI).

vhdl_index_subtype_definitions([]) ::= [')'].

vhdl_index_subtype_definitions([I|Is]) ::=
        [','], vhdl_index_subtype_definition(I),
        vhdl_index_subtype_definitions(Is).
/**
 
\subsubsection{Rule 55}
**/
vhdl_index_subtype_definition(TM) ::=
        vhdl_mark(TM), [range,'<','>'].
/**
 
\subsubsection{Rule 56}
**/
vhdl_constrained_array_definition(vhdl_array(IC,SI)) ::=
        [array], vhdl_index_constraint(IC),
        [of], vhdl_subtype_indication(SI).
/**
 
\subsubsection{Rule 57}
**/
vhdl_record_type_definition(record(EDs)) ::=
       [ record ],
        vhdl_element_declarations(EDs),
       [ end, record ].
/**
 
 
\subsubsection{Rule 58}
**/
vhdl_element_declaration(vhdl_element(Is,SI)) ::=
       vhdl_identifier_list(Is),
       [':'],
       vhdl_subtype_indication(SI).
/**
**/
vhdl_element_declarations([E|Es]) ::=
       vhdl_element_declaration(E),
       vhdl_element_declarations(Es).

vhdl_element_declarations([],L,L).
/**
\subsubsection{Rule 59}
**/
vhdl_access_type_definition(vhdl_access(TD)) ::=
       [access], vhdl_subtype_indication(TD).
/**
 
\subsubsection{Rule 60}
**/
vhdl_file_type_definition(vhdl_filetype(Type)) ::=
       [ file, of ], vhdl_mark(Type).
/**
 
 
\subsection{Subtypes and Constraints}
\subsubsection{Rule 61}
**/
vhdl_subtype_declaration ::=
       [ subtype ], vhdl_identifier(ID),
       [is], vhdl_subtype_indication(TD).
/**
 
\subsubsection{Rule 62}
**/
vhdl_subtype_indication(subtype(TM,C))::=
       vhdl_opt_mark(FM),
       vhdl_mark(TM),
       vhdl_opt_constraint(C).
/**
 
\subsubsection{Rule 63}
**/
vhdl_opt_constraint(C)    ::= vhdl_constraint(C).
vhdl_opt_constraint(null) ::= [].

vhdl_constraint(R) ::= vhdl_range_constraint(R).
vhdl_constraint(I) ::= vhdl_index_constraint(I).
/**
 
\subsubsection{Rule 64}
**/
vhdl_range_constraint(S) ::=
       [range], vhdl_range_specification(S).
/**
 
\subsubsection{Rule 65}
Once again, if we lead with the test for the
close parenthesis ({\tt ')'}) we can save
re-parsing the final discrete-range element.
**/
vhdl_index_constraint(index([R|Rs])) ::=
       ['('], vhdl_discrete_range(R),
              vhdl_more_discrete_ranges(Rs).

vhdl_more_discrete_ranges([]) ::= [')'].
vhdl_more_discrete_ranges([R|Rs]) ::=
        [','], vhdl_discrete_range(R), vhdl_more_discrete_ranges(Rs).
/**
 
\subsubsection{Rule 66}
**/
vhdl_discrete_range(R) ::= vhdl_range_specification(R).

vhdl_discrete_range(R) ::= vhdl_subtype_indication(R).
/**
 
\subsubsection{Rule 67}
For the range specification below, the
grammar in {\it VHDL: Hardware Description and Design}
does not include the rule for an
indexed name followed by an optional static expression.
However, I have found examples of VHDL which seem to
require this.  In any case, this rule should be viewed
with suspicion.
**/
%vhdl_range_specification(vhdl_range(vhdl_name(P,Exprs,range),SE)) ::=
%       vhdl_indexed_name(vhdl_name(P,Exprs,[range])),
%       vhdl_opt_static_expression(SE).
%
%vhdl_range_specification(vhdl_range(TM,SE)) ::=
%       vhdl_mark(TM), [ attr(range) ], vhdl_opt_static_expression(SE).
%
vhdl_range_specification(range(E1,E2)) ::=
       expression(E1), vhdl_direction(D), expression(E2).
/**
 
\subsubsection{Rule 68}
**/
vhdl_direction(to)     ::= [to].
vhdl_direction(downto) ::= [downto].
/**
 
\subsection{Objects, Aliases, Files, Disconnections}
\subsubsection{Rule 69}
**/
vhdl_object_declaration ::=
       vhdl_object_class(Class),
       vhdl_identifier_list(IDs0, [], IDs0), [':'],
       vhdl_subtype_indication(Type),
       vhdl_opt_signal_kind(_Kind),
       vhdl_opt_assignment(Expr),
       updates(data(IDs0, Type, Expr)),
       update(object(IDs0, Class)).

/**
 
**/
vhdl_opt_assignment(vhdl_assign(Expr)) ::=
       [':='], expression(Expr).

vhdl_opt_assignment(null) ::= [].
/**
 
\subsubsection{Rule 70}
**/
vhdl_object_class(signal)   ::= [signal].
vhdl_object_class(constant) ::= [constant].
vhdl_object_class(variable) ::= [variable].
/**
 
**/
vhdl_opt_object_class(Class) ::= vhdl_object_class(Class).
vhdl_opt_object_class(null)  ::= [].
/**
 
\subsubsection{Rule 71}
**/
vhdl_opt_signal_kind(bus)      ::= [bus].
vhdl_opt_signal_kind(register) ::= [register].
vhdl_opt_signal_kind(null)     ::= [].
/**
 
\subsubsection{Rule 72}
**/
vhdl_alias_declaration(vhdl_alias(ID,Type,Name)) ::=
       [alias], vhdl_identifier(ID), [':'],
       vhdl_subtype_indication(Type),  [is], vhdl_name(Name).
/**
 
\subsubsection{Rule 73}
**/
vhdl_file_declaration(vhdl_file(ID,SI,Mode,Se)) ::=
       [file], 
          vhdl_identifier(ID),
          vhdl_subtype_indication(SI),
       [ is ],
          vhdl_opt_mode(Mode), [ string(Se) ].
/**
\subsubsection{Rule 74}
**/
vhdl_disconnection_specification(disconnect_spec(SL,Type,Time)) ::=
       [ disconnect ],
          vhdl_signal_list(SL), [':'], vhdl_mark(Type),
       [ after ], expression(Time).
/**
 
\subsubsection{Rule 75}
**/
vhdl_signal_list(all)    ::= [all].

vhdl_signal_list(others) ::= [others].

vhdl_signal_list([S|Ss]) ::=
        vhdl_name(S), [','],
        vhdl_signal_list(Ss).

vhdl_signal_list([S])    ::= vhdl_name(S).
/**
 
\subsection{Concurrent Statements}
\subsubsection{Rule 86}

**/
vhdl_concurrent_statements ::=
        vhdl_concurrent_statement, 
        vhdl_concurrent_statements.

vhdl_concurrent_statements ::= [].
/**
 
\subsubsection{Rule 87}
**/
%vhdl_concurrent_statement ::=
%          vhdl_process_statement.
%vhdl_concurrent_statement(S) ::=
%          vhdl_block_statement(S).
%vhdl_concurrent_statement(S) ::=
%          vhdl_concurrent_assertion_statement(S).
%vhdl_concurrent_statement(S) ::=
%          vhdl_concurrent_procedure_call(S).
vhdl_concurrent_statement ::=
          vhdl_component_instantiation_statement.
vhdl_concurrent_statement ::=
          vhdl_concurrent_signal_assignment_statement.
%vhdl_concurrent_statement(S) ::=
%          vhdl_generate_statement(S).
/**

 
\subsubsection{Rule 88}
**/
vhdl_block_statement(block(ID,Expr,GIL,GAL,PIL,PAL,ODI,Ss)) ::=
        vhdl_identifier(ID), [ ':', block ],
           vhdl_opt_paren_expression(Expr),
           vhdl_opt_generic_statement(GIL),
           vhdl_opt_generic_map_statement(GAL),
           vhdl_opt_port_statement(PIL),
           vhdl_opt_port_map_statement(PAL),
           vhdl_opt_declarative_items(ODI),
        [ begin ],
           vhdl_concurrent_statements(Ss),
        [ end, block ], vhdl_opt_identifier(ID).

/**
\subsubsection{Rule 89}
**/
vhdl_component_instantiation_statement ::=
        vhdl_identifier(ID), [ ':' ],  vhdl_mark(CM),
        vhdl_opt_generic_map_statement,
        vhdl_opt_port_map_statement(FormalPorts, ActualPorts),
        update(instance(ID, CM)),
        update(formal_instance_ports(FormalPorts,ID)),
        update(actual_instance_ports(ActualPorts,ID)).
/**
 
\subsubsection{Rule 90}
**/
vhdl_concurrent_assertion_statement(concurrent_assertion(ID,Stmt)) ::=
       vhdl_opt_label(ID), vhdl_assertion_statement(Stmt).
/**
 
\subsubsection{Rule 91}
**/
vhdl_concurrent_procedure_call(cpc(ID,S)) ::=
       vhdl_opt_label(ID), vhdl_procedure_call_statement(S).
/**
 
\subsubsection{Rule 92}
**/
vhdl_concurrent_signal_assignment_statement ::=
        vhdl_opt_label(ID),
        vhdl_conditional_signal_assignment.
%vhdl_concurrent_signal_assignment_statement ::=
%        vhdl_opt_label(ID),
%        vhdl_selected_signal_assignment_statement.
%vhdl_concurrent_signal_assignment_statement ::= [].
/**
\subsubsection{Rule 93}
**/
vhdl_conditional_signal_assignment ::=
       vhdl_target(T), ['<='],
       vhdl_options(G,Tr), vhdl_conditional_waveforms(T),
       [';'].
/**
 
\subsubsection{Rule 94}
**/
vhdl_conditional_waveforms(Target) ::=        
        down(Treeif),
        vhdl_waveform(Target), 
        up,
        down(TreeElse),
        vhdl_opt_conditional_waveforms(Target, Cond),
        up,
        update(assign_concurrent(if(Cond,Treeif,TreeElse))).

vhdl_opt_conditional_waveforms(Target, Expr) ::=
       [ when ], expression(Expr),
       [ else ],    vhdl_conditional_waveforms(Target).

vhdl_opt_conditional_waveforms(_, true) ::= [].
/**
 
\subsubsection{Rule 95}
**/
vhdl_waveform(Target) ::=
       vhdl_waveform_element(Target), vhdl_opt_waveforms(Target).

vhdl_opt_waveforms(Target) ::=
     [','], vhdl_waveform_element(Target), vhdl_opt_waveforms(Target).
vhdl_opt_waveforms(_) ::= [].
/**
 
\subsubsection{Rule 96}
**/
vhdl_waveform_element(Target) ::=
       [ null ], vhdl_opt_delay(Time),
	update(delay(Target, Time)),
        update(signal(Target,Time,null)).
vhdl_waveform_element(Target) ::=
       expression(Expr), vhdl_opt_delay(Time),
	update(delay(Target, Time)),
       update(signal_assign(Target,Time,Expr)).

vhdl_opt_delay(Time) ::= [ after ], expression(Time).
vhdl_opt_delay(null) ::= [].
/**
 
\subsubsection{Rule 97}
**/
vhdl_target(N) ::= vhdl_name(N).
%vhdl_target(A) ::= vhdl_aggregate(A).
/**
 
\subsubsection{Rule 98}
**/
vhdl_options(guarded,T)   ::= [  guarded  ], vhdl_options(_,T).
vhdl_options(null,transport) ::= [ transport ].
vhdl_options(null,null)   ::= [].
/**
 
\subsubsection{Rule 99}
**/
vhdl_selected_signal_assignment_statement(selected(Expr,Target,G,T,Ws)) ::=
        [ with ], expression(Expr), [ select ],        
        vhdl_target(Target),
        ['<='], vhdl_options(G,T), vhdl_selected_waveforms(Ws),
       [';'].
/**
 
\subsubsection{Rule 100}
**/
vhdl_selected_waveform(vhdl_waveform(W,Cs)) ::=
        vhdl_waveform(W), [ when ], vhdl_choices(Cs).

vhdl_selected_waveforms([SW|SWs]) ::=
        vhdl_selected_waveform(SW),
        vhdl_additional_selected_waveforms(SWs).

vhdl_additional_selected_waveforms(SWs) ::=
         [','], vhdl_selected_waveforms(SWs).

vhdl_additional_selected_waveforms([]) ::= [].
/**
 
\subsubsection{Rule 101}
**/
vhdl_generate_statement(generate(ID,Scheme,Ss)) ::=
        vhdl_identifier(ID),  [':'],
           vhdl_generation_scheme(Scheme), [ generate ],
           vhdl_concurrent_statements(Ss),
        [ end, generate ], vhdl_opt_identifier(ID).
/**
 
\subsubsection{Rule 102}
**/
vhdl_process_statement ::=
        vhdl_opt_label(ID),
        [ process ], vhdl_opt_sensitivity_list(SL),
        vhdl_opt_declarative_items,
        [ begin ],
        down(Ss),
        vhdl_sequential_statements,
        up,
        [ end , process ], vhdl_opt_identifier(ID), [';'],
        seq_update(subcell(ID,process_stmt,Ss)).
%       update(process_statement(ID)).

vhdl_opt_label(ID)   ::= vhdl_identifier(ID), [':'].
vhdl_opt_label(null) ::= [].
/**
 
\subsubsection{Rule 103}
**/
vhdl_sensitivity_list([S|Ss]) ::=
        vhdl_name(S), [','],  vhdl_sensitivity_list(Ss).
vhdl_sensitivity_list([S]) ::= vhdl_name(S).

vhdl_opt_sensitivity_list(SL) ::= ['('], vhdl_sensitivity_list(SL), [')'].
vhdl_opt_sensitivity_list([]) ::= [].
/**
 
\subsection{Sequential Statements}
When more than one semi-colon terminated statement is
part of a production, we must call {\tt vhdl\_get\_more//0}
to read more tokens.
\subsubsection{Rule 104}
**/
vhdl_sequential_statements ::=
        vhdl_sequential_statement, [';'],
        vhdl_sequential_statements.

vhdl_sequential_statements ::= [].
/**
\subsubsection{Rule 105}
Before giving the proper definition of the sequential statement,
we include a special rule for debugging.
Debugging this grammar on large VHDL files will
be problematic unless we have a way of signaling, from 
inside the text of the VHDL file, when we want to turn on tracing.
To this end we will pretend that VHDL has a {\tt TRACE} 
statement, consisting of the keyword {\tt trace} followed
by a semicolon. Whenever the parser encounters this
special sequential statement, tracing will be turned on.
The object returned to the parser will be the same as for the
null statement.  This clause, and the {\tt trace} keyword,
should probably be removed in a production version of the parser.
**/
%vhdl_sequential_statement(null) ::= [trace]. %, {trace}.
%vhdl_sequential_statement(S) ::= vhdl_assertion_statement(S).
%vhdl_sequential_statement(S) ::= vhdl_case_statement(S).
%vhdl_sequential_statement(S) ::= vhdl_exit_statement(S).
%vhdl_sequential_statement(S) ::= vhdl_if_statement(S).
%vhdl_sequential_statement(S) ::= vhdl_loop_statement(S).
%vhdl_sequential_statement(S) ::= vhdl_next_statement(S).
%vhdl_sequential_statement(S) ::= vhdl_null_statement(S).
%vhdl_sequential_statement(S) ::= vhdl_procedure_call_statement(S).
%vhdl_sequential_statement(S) ::= vhdl_return_statement(S).
vhdl_sequential_statement ::= vhdl_signal_assignment_statement.
vhdl_sequential_statement ::= vhdl_variable_assignment_statement.
%vhdl_sequential_statement(S) ::= vhdl_wait_statement(S).
/**
 
\subsubsection{Rule 106}
**/
vhdl_assertion_statement(assert(Assertion,String,Severity)) ::=
       [ assert ],  expression(Assertion),
                    vhdl_opt_report(String),
                    vhdl_opt_severity(Severity).

vhdl_opt_report(R)      ::= [ report ],  expression(R).
vhdl_opt_report(null)   ::= [].

vhdl_opt_severity(S)    ::= [ severity ], expression(S).
vhdl_opt_severity(null) ::= [].
/**
 
\subsubsection{Rule 107}
**/
vhdl_case_statement(case(Cond,Cases)) ::=
       [ case ], expression(Cond), [ is ],
       vhdl_case_statement_alternatives(Cases),
       [ end, case ].
/**
 
\subsubsection{Rule 108}
**/
vhdl_case_statement_alternatives([vhdl_case(Choices,Ss)|Cases]) ::=
       [ when ],  vhdl_choices(Choices), ['=>'],
       vhdl_sequential_statements(Ss),
       vhdl_case_statement_alternatives(Cases).

vhdl_case_statement_alternatives([]) ::= [].
/**
 
\subsubsection{Rule 109}
**/
vhdl_exit_statement(vhdl_exit(ID,Expr)) ::=
       [ exit ], vhdl_identifier(ID),
       [ when ], expression(Expr).
/**
\subsubsection{Rule 110}
The cut after the {\tt then} is necessary to
prevent an erroneous sequential statement from being
discarded.  Since {\tt expression/1} can usually
be satisfied more than once, backtracking to before the
then clause will succeed and proceed to parse sequential
statemnts which previously have read (and thus discarded)
an erroneous statement. Note that the syntax error will be
caught if it is the first sequential statement (e.g. before
the first semi-colon), but that subsequent sequential 
statements might be discarded.  For want of a cut a kingdom 
might be lost indeed!
**/
vhdl_if_statement(vhdl_if(Cond,Thens,Elses)) ::=
       [ if ], expression(Cond), 
         [ then ], vhdl_sequential_statements(Thens),
         vhdl_else(Elses).

vhdl_else(elif(ECond,Thens,Else)) ::=
            [ elsif ],
               expression(ECond),
             [ then ],
               vhdl_sequential_statements(Thens),
             vhdl_else(Else).

vhdl_else(Elses) ::=
            [ else ], vhdl_sequential_statements(Elses),
            vhdl_end.

vhdl_else([]) ::= vhdl_end.

vhdl_end ::= [ end, if ].
/**
 
\subsubsection{Rule 111}
**/
vhdl_loop_statement(vhdl_loop(ID,IScheme,Ss)) ::=
        vhdl_opt_label(ID),
        vhdl_opt_iteration_scheme(IScheme),
        [ loop ],
          vhdl_sequential_statements(Ss),
        [ end, loop ], vhdl_opt_identifier(ID).
/**
 
\subsubsection{Rule 112}
**/
vhdl_next_statement(vhdl_next(ID,Expr)) ::=
       [ next ], vhdl_identifier(ID),
       [ when ], expression(Expr).
/**
 
\subsubsection{Rule 113}
**/
vhdl_null_statement(null) ::= [null].
/**
 
\subsubsection{Rule 114}
**/
vhdl_procedure_call_statement(vhdl_call(ID,Args)) ::=
       vhdl_mark(ID), vhdl_opt_association_list(Args).
/**
 
\subsubsection{Rule 115}
**/
vhdl_return_statement(return(Expr)) ::=
       [ return ], expression(Expr).
/**
 
\subsubsection{Rule 116}
**/
vhdl_signal_assignment_statement ::=
       vhdl_target(T), ['<='],
       vhdl_opt_transport(TR), vhdl_waveform(W),
       update(assign_sig(T,options(TR),W)).    

vhdl_opt_transport(transport) ::= [transport].
vhdl_opt_transport(null)      ::= [].
/**
 
\subsubsection{Rule 117}
**/
vhdl_variable_assignment_statement ::=
       vhdl_target(Var), [ ':=' ], expression(Expr),
       update(assign(Var,Expr)).
/**
 
\subsubsection{Rule 118}
**/
vhdl_wait_statement(vhdl_wait(SL,Expr,Time)) ::=
       [ wait ],  vhdl_opt_on(SL),
       vhdl_opt_until(Expr),
       vhdl_opt_for_time(Time).

vhdl_opt_on(SL)      ::= [ on ],vhdl_sensitivity_list(SL).
vhdl_opt_on(null)    ::= [].

vhdl_opt_until(Expr) ::= [ until ],expression(Expr).
vhdl_opt_until(null) ::= [].

vhdl_opt_for_time(Time) ::= [ for ],expression(Time).
vhdl_opt_for_time(0) ::= [].
/**
 
\subsection{Components and Configurations}
\subsubsection{Rule 119}
**/
vhdl_component_declaration ::=
       [ component ], vhdl_identifier(ID),
        down(Comp),
         vhdl_opt_generic_statement,         
         vhdl_opt_port_statement,
        up,
       [ end, component],
       update(component_decl(ID, Comp)).
/**
 
 
\subsubsection{Rule 120}
**/
vhdl_block_configuration(vhdl_conf(Spec,Use,CI)) ::=
       [ for ], vhdl_block_specification(Spec),
          vhdl_use_clause(Use),
          vhdl_configuration_item(CI),
       [ end, for ].
/**
 
 
\subsubsection{Rule 121}
**/
vhdl_block_specification(AM) ::=
       vhdl_mark(AM).

vhdl_block_specification(ID) ::=
       vhdl_identifier(ID).

vhdl_block_specification(vhdl_name(ID,Index)) ::=
       vhdl_identifier(ID),
       vhdl_index_specification(Index).
/**
 
 
\subsubsection{Rule 122}
**/
vhdl_index_specification(Range) ::= vhdl_discrete_range(Range).

vhdl_index_specification(Expr) ::= expression(Expr).
/**
 
 
\subsubsection{Rule 123}
**/
vhdl_configuration_item(BC) ::= vhdl_block_configuration(BC).

vhdl_configuration_item(CC) ::= vhdl_component_configuration(CC).
/**
 
 
\subsubsection{Rule 124}
**/
vhdl_component_configuration(vhdl_spec(CSpec,BI,Ss)) ::=
       [ for ], vhdl_component_specification(CSpec),
         [ use ], vhdl_binding_indication(BI),
              vhdl_block_configuration(Ss),
       [ end, for ].
/**
\subsubsection{Rule 125}
**/
vhdl_configuration_specification(vhdl_spec(CSpec,BI)) ::=
       [ for ], vhdl_component_specification(CSpec),
       [ use ], vhdl_binding_indication(BI).
/**
\subsubsection{Rule 126}
**/
vhdl_component_specification(spec(Spec,ID)) ::=
       vhdl_instantiation_list(Spec), [':'], vhdl_mark(ID).
/**
 
 
\subsubsection{Rule 127}
**/
vhdl_instantiation_list(all)    ::= [all].
vhdl_instantiation_list(others) ::= [others].
vhdl_instantiation_list(IL)     ::= vhdl_identifier_list(IL).
/**
 
\subsubsection{Rule 128}
**/
vhdl_binding_indication(binding(EA,GAL,PAL)) ::=
       vhdl_entity_aspect(EA),
         vhdl_opt_generic_map_statement(GAL),
         vhdl_opt_port_map_statement(PAL).
/**
**/
vhdl_opt_generic_map_statement ::=
        [ generic, map ],
        vhdl_association_list(Name,Value).
vhdl_opt_generic_map_statement ::= [].
/**
**/
vhdl_opt_port_map_statement(FValues, AValues) ::=
        [ port, map ],
        vhdl_association_list(FValues, AValues).
vhdl_opt_port_map_statement([],[]) ::= [].
/**
 
\subsubsection{Rule 129}
**/
vhdl_entity_aspect(open) ::= [open].

vhdl_entity_aspect(entity_aspect(CM,null)) ::=
       [configuration], vhdl_mark(CM).

vhdl_entity_aspect(entity_aspect(Mark,ID)) ::=
       [ entity],  vhdl_mark(Mark),
                   vhdl_opt_arch_identifier(ID).

vhdl_opt_arch_identifier(ID)   ::= ['('], vhdl_identifier(ID), [')'].
vhdl_opt_arch_identifier(null) ::= [].

/**
 
\subsection{Grammar Rule Index}
The following alphabetic index will help in
locating the grammar rules by name.
\vskip 1.0cm

Rule\ \ \  3\ \ \ \ \ vhdl\_abstract\_literal//1

Rule\ \  59\ \ \ \ \ vhdl\_access\_type\_definition//1

Rule\ \  27\ \ \ \ \ vhdl\_actual\_part//1

Rule\ \  39\ \ \ \ \ vhdl\_aggregate//1

Rule\ \  72\ \ \ \ \ vhdl\_alias\_declaration//1

Rule\ \  42\ \ \ \ \ vhdl\_allocator//1

Rule\ \  13\ \ \ \ \ vhdl\_architecture\_body//1

Rule\  106\ \ \ \ \ vhdl\_assertion\_statement//1

Rule\ \  25\ \ \ \ \ vhdl\_association\_element//1

Rule\ \  24\ \ \ \ \ vhdl\_association\_list//1

Rule\ \  76\ \ \ \ \ vhdl\_attribute\_declaration//1

Rule\ \  37\ \ \ \ \ vhdl\_attribute\_name//1

Rule\ \  52\ \ \ \ \ vhdl\_base\_unit\_declaration//1

Rule\ \  77\ \ \ \ \ vhdl\_attribute\_specification//1

Rule\ \  43\ \ \ \ \ vhdl\_binary\_operator//2

Rule\  128\ \ \ \ \ vhdl\_binding\_indication//3

Rule\  120\ \ \ \ \ vhdl\_block\_configuration//1

Rule\  121\ \ \ \ \ vhdl\_block\_specification//1

Rule\ \  88\ \ \ \ \ vhdl\_block\_statement//1

Rule\  107\ \ \ \ \ vhdl\_case\_statement//1

Rule\  108\ \ \ \ \ vhdl\_case\_statement\_alternatives//1\ 

Rule\ \  47\ \ \ \ \ vhdl\_choice//1

Rule\ \  46\ \ \ \ \ vhdl\_choices//1

Rule\  124\ \ \ \ \ vhdl\_component\_configuration//1

Rule\  119\ \ \ \ \ vhdl\_component\_declaration//1

Rule\ \  89\ \ \ \ \ vhdl\_component\_instantiation\_statement//1

Rule\  126\ \ \ \ \ vhdl\_component\_specification//1

Rule\ \  90\ \ \ \ \ vhdl\_concurrent\_assertion\_statement//1

Rule\ \  91\ \ \ \ \ vhdl\_concurrent\_procedure\_call//1

Rule\ \  92\ \ \ \ \ vhdl\_concurrent\_signal\_assignment\_statement//1

Rule\ \  87\ \ \ \ \ vhdl\_concurrent\_statement//1

Rule\ \  86\ \ \ \ \ vhdl\_concurrent\_statements//1

Rule\ \  93\ \ \ \ \ vhdl\_conditional\_signal\_assignment//1

Rule\ \  94\ \ \ \ \ vhdl\_conditional\_waveforms//1

Rule\ \  14\ \ \ \ \ vhdl\_configuration\_declaration//1

Rule\  123\ \ \ \ \ vhdl\_configuration\_item//1

Rule\  125\ \ \ \ \ vhdl\_configuration\_specification//1

Rule\ \  56\ \ \ \ \ vhdl\_constrained\_array\_definition//1

Rule\ \  17\ \ \ \ \ vhdl\_declarative\_item//1

Rule\ \ \  7\ \ \ \ \ vhdl\_design\_unit//1

Rule\ \ \  1\ \ \ \ \ vhdl\_designator//1

Rule\ \  68\ \ \ \ \ vhdl\_direction//1

Rule\ \  74\ \ \ \ \ vhdl\_disconnection\_specification//1

Rule\ \  66\ \ \ \ \ vhdl\_discrete\_range//1

Rule\ \  45\ \ \ \ \ vhdl\_element\_association//1

Rule\ \  58\ \ \ \ \ vhdl\_element\_declaration//1

Rule\  129\ \ \ \ \ vhdl\_entity\_aspect//1

Rule\ \  80\ \ \ \ \ vhdl\_entity\_class//1

Rule\ \  12\ \ \ \ \ vhdl\_entity\_declaration//1

Rule\ \  79\ \ \ \ \ vhdl\_entity\_name\_list//1

Rule\ \  78\ \ \ \ \ vhdl\_entity\_specification//1

Rule\ \ \  4\ \ \ \ \ vhdl\_enumeration\_literal//1

Rule\ \  50\ \ \ \ \ vhdl\_enumeration\_type\_definition//1

Rule\  109\ \ \ \ \ vhdl\_exit\_statement//1

Rule\ \  29a\ \ \ \ vhdl\_expression//1

Rule\ \  29b\ \ \ \ vhdl\_expression//3

Rule\ \  29c\ \ \ \ vhdl\_expression//5

Rule\ \  73\ \ \ \ \ vhdl\_file\_declaration//1

Rule\ \  60\ \ \ \ \ vhdl\_file\_type\_definition//1

Rule\ \  84\ \ \ \ \ vhdl\_for\_scheme//1

Rule\ \  26\ \ \ \ \ vhdl\_formal\_part//1

Rule\ \  38\ \ \ \ \ vhdl\_function\_call//1

Rule\  101\ \ \ \ \ vhdl\_generate\_statement//1

Rule\ \  81\ \ \ \ \ vhdl\_generation\_scheme//1

Rule\ \ \  6\ \ \ \ \ vhdl\_identifier//1

Rule\ \  83\ \ \ \ \ vhdl\_if\_scheme//1

Rule\  110\ \ \ \ \ vhdl\_if\_statement//1

Rule\ \  65\ \ \ \ \ vhdl\_index\_constraint//1

Rule\  122\ \ \ \ \ vhdl\_index\_specification//1

Rule\ \  55\ \ \ \ \ vhdl\_index\_subtype\_definition//1

Rule\ \  35\ \ \ \ \ vhdl\_indexed\_name//1

Rule\  127\ \ \ \ \ vhdl\_instantiation\_list//1

Rule\ \  22\ \ \ \ \ vhdl\_interface\_element//1

Rule\ \  82\ \ \ \ \ vhdl\_iteration\_scheme//1

Rule\ \  10\ \ \ \ \ vhdl\_library\_clause//1

Rule\ \ \  8\ \ \ \ \ vhdl\_library\_unit//1

Rule\ \ \  2\ \ \ \ \ vhdl\_literal//1

Rule\  111\ \ \ \ \ vhdl\_loop\_statement//1

Rule\ \  28\ \ \ \ \ vhdl\_mark//1

Rule\ \  31\ \ \ \ \ vhdl\_name//1

Rule\  112\ \ \ \ \ vhdl\_next\_statement//1

Rule\  113\ \ \ \ \ vhdl\_null\_statement//1

Rule\ \  70\ \ \ \ \ vhdl\_object\_class//1

Rule\ \  69\ \ \ \ \ vhdl\_object\_declaration//1

Rule\ \  63\ \ \ \ \ vhdl\_opt\_constraint//1

Rule\ \ \  9\ \ \ \ \ vhdl\_opt\_context\_items//1

Rule\ \  21\ \ \ \ \ vhdl\_opt\_interface\_list//1

Rule\ \  23\ \ \ \ \ vhdl\_opt\_mode//1

Rule\ \  71\ \ \ \ \ vhdl\_opt\_signal\_kind//1

Rule\ \  98\ \ \ \ \ vhdl\_options//1

Rule\ \  16\ \ \ \ \ vhdl\_package\_body//1

Rule\ \  15\ \ \ \ \ vhdl\_package\_declaration//1

Rule\ \ \  5\ \ \ \ \ vhdl\_physical\_literal//1

Rule\ \  51\ \ \ \ \ vhdl\_physical\_type\_definition//1

Rule\ \  33\ \ \ \ \ vhdl\_prefix//1

Rule\ \  30\ \ \ \ \ vhdl\_primary//1

Rule\  114\ \ \ \ \ vhdl\_procedure\_call\_statement//1

Rule\  102\ \ \ \ \ vhdl\_process\_statement//1

Rule\ \  40\ \ \ \ \ vhdl\_qualified\_expression//1

Rule\ \  64\ \ \ \ \ vhdl\_range\_constraint//1

Rule\ \  67\ \ \ \ \ vhdl\_range\_specification//1

Rule\ \  57\ \ \ \ \ vhdl\_record\_type\_definition//1

Rule\  115\ \ \ \ \ vhdl\_return\_statement//1

Rule\ \  53\ \ \ \ \ vhdl\_secondary\_unit\_declaration//1

Rule\ \  32\ \ \ \ \ vhdl\_selected\_name//1

Rule\ \  99\ \ \ \ \ vhdl\_selected\_signal\_assignment\_statement//1

Rule\  100\ \ \ \ \ vhdl\_selected\_waveform//1

Rule\  103\ \ \ \ \ vhdl\_sensitivity\_list//1

Rule\  105\ \ \ \ \ vhdl\_sequential\_statement//1

Rule\  104\ \ \ \ \ vhdl\_sequential\_statements//1

Rule\  116\ \ \ \ \ vhdl\_signal\_assignment\_statement//1

Rule\ \  75\ \ \ \ \ vhdl\_signal\_list//1

Rule\ \  36\ \ \ \ \ vhdl\_slice\_name//1

Rule\ \  20\ \ \ \ \ vhdl\_subprogram\_body//1

Rule\ \  18\ \ \ \ \ vhdl\_subprogram\_declaration//1

Rule\ \  19\ \ \ \ \ vhdl\_subprogram\_specification//1

Rule\ \  61\ \ \ \ \ vhdl\_subtype\_declaration//1

Rule\ \  62\ \ \ \ \ vhdl\_subtype\_indication//1

Rule\ \  34\ \ \ \ \ vhdl\_suffix//1

Rule\ \  97\ \ \ \ \ vhdl\_target//1

Rule\ \  41\ \ \ \ \ vhdl\_type\_conversion//1

Rule\ \  48\ \ \ \ \ vhdl\_type\_declaration//1

Rule\ \  49\ \ \ \ \ vhdl\_type\_definition//1

Rule\ \  44\ \ \ \ \ vhdl\_unary\_operator//1

Rule\ \  54\ \ \ \ \ vhdl\_unconstrained\_array\_definition//1

Rule\ \  11\ \ \ \ \ vhdl\_use\_clause//1

Rule\  117\ \ \ \ \ vhdl\_variable\_assignment\_statement//1

Rule\  118\ \ \ \ \ vhdl\_wait\_statement//1

Rule\ \  95\ \ \ \ \ vhdl\_waveform//1

Rule\ \  96\ \ \ \ \ vhdl\_waveform\_element//1

Rule\ \  85\ \ \ \ \ vhdl\_while\_scheme//1
**/
