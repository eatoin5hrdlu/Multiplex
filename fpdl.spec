/**
% FPDL FORMAT FILE               WARNING:    THIS FILE CONTAINS AT\&T
%                                PROPRIETARY  INFORMATION  ABOUT THE
% Copyright 1991 by INTERGRAPH   FPDL LANGUAGE  AND  SHOULD  NOT  BE
% Author:  Peter B. Reintjes     CONSIDERED AS DASIX SOURCE CODE.
\documentstyle[11pt]{article}
\hfuzz=8pt
\begin{document}
\title{{\LARGE FPDL:} A Grammar for MULTIPLEX}
\author{Peter B. Reintjes}
\date{\today}
\maketitle
\vskip 1.0cm
\centerline{\LARGE DRAFT}
\newpage
\section{FPDL Lexical Analyzer}
**/
fpdl lexicon

"\+"     is    '+';
"\."    is    '.';
":"     is    ':';
";"     is    ';';
","     is    ',';
"\-"     is    '-';
"\("    is    '(';
"\)"    is    ')';
"="     is    '=';
"\["    is    '[';
"\]"    is    ']';
"&"     is    '&';
"\|"     is    '|';
"~"     is    '~';

"\*"     is   Next if (scan_past(";"), switch(fpdl,Next));

'"[^"]*"'            is quote(double(Q))
                     if (text(T), append([0'"|Q],[0'"],T));
"'[^']*'"            is quote(single(Q))
                     if (text(T),append([0''|Q],[0''],T));
"[Oo][0-7]+"                  is number(N,8,"O","")
                              if (text(T),convert(8,N,T));
"[Xx][0-9A-Fa-f]+"            is number(N,16,"X","")
                              if (text(T),convert(16,N,T));
"-?[0-9]+"                    is number(N)
                              if (text(T),number_chars(N,T));
"\.[0-9]+([eE][+-]?[0-9]+)?"  is number(F)
                              if number_chars(F,[0'0|text]);
"-\.[0-9]+([eE][+-]?[0-9]+)?" is number(F) if ( text = [C|Cs],
                                                number_chars(F,[C,0'0|Cs]));
"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?" is number(F)
                                     if (text(T),number_chars(F,T));
"[a-zA-Z_][a-zA-Z_0-9<>./$#+-]*" is N
                                 if (text(T),atom_chars(N,T));
"[ \t\n]"                        is R if switch(fpdl,R).

/**
\section{FPDL Grammar Rules}
The first rule "fpdl", specifies that a complete AT\&T Synthesis
description may consist of three files, and that the data
in all three files conforms to the lexical specification
"fpdl". In these first rules, the file//2 directive says that
the rules after it will expect tokenized data from (send
generated output to) the file "name.suf", where "name" will
be substituted with the root of the input (output) file-name.
\begin{verbatim}
            file(name.suf, lex), rule1, rule2, ...
\end{verbatim}
The -name option on the command line can be used to specify
an alternate substitution for name if it is not obvious
from context.  Similarly, "suf" and "lex" specify the
language and lexical ruleset to use while "rule" specifies the 
grammar rule which matches the contents of the file. The two
strings "name" and "lang" are substituted automatically by
the program but any other substitutions can be specified
by command line options. The option "-par pb" will cause
"pb" to be substituted for "par" in file//2 directives.

The command line: "translate foo.syn bar.fpdl" will
result in the following version of the "fpdl" rule:
\begin{verbatim}
  fpdl ::= file('chars_par', fpdl), parameters,
           file('foo.env',   fpdl), environment,
           file('foo.fpdl',  fpdl), library.
\end{verbatim}
While: {\tt translate -par pb -fpdl fpd  foo.syn bar.fpdl}
will result in:
\begin{verbatim}
  fpdl ::= file('chars.pb', fpdl), parameters,
          file('foo.env',  fpdl), environment,
          file('foo.fpd',  fpdl), library.
\end{verbatim} 
Thus, command line operations can override file names and suffixes,
but not lexicon specifiers (which refer to a lexer in the same file).
**/

fpdl ::= down(Lib),
          fpdl_files,
         up,
         { multi:current_name(Name) ; true },
         update(subcell(Name, library, Lib)).

fpdl_files ::= opt_parameter_file,
               opt_environment_file,
               file('name.fpdl', fpdl),
               cell,
               more_cells.
/**
**/
opt_parameter_file   ::= file('chars.par', fpdl), parameters.
opt_parameter_file   ::= [].

opt_environment_file ::= file('name.env',  fpdl), environment.
opt_environment_file ::= [].

parameters ::= parameter, newline, parameters.
parameters ::= [].

parameter ::= ['TEMPERATURE', =, Value,';'],
              update(nom_temperature(Value)).

/**
**/
environments ::= environment, newline, environments.
environments ::= [].

environment ::= ['VOLTAGE', =, Value,';'], update(nom_voltage(Value)).

more_cells ::= cell, more_cells.
more_cells ::= [].

/**
**/

cell ::= down(Cell),
           cell_body(Name),
         up,
         update(subcell(Name,cell,Cell)).

cell_body(N)  ::= circuit, [:], name(N), [';'], newline,
                  { write(fpdl_cell(N)),nl },
                  declarations,
                  begin, [:],
                  indent,
                    statements,
                  undent,
                  end, [';'], newline, newline.

cell_body(N)  ::= circuit, [:], name(N), [';'], newline,
                  declarations,
                  begin, [:], 
                    statements,
                  parse(error).

cell_body(N)  ::= circuit, [:], name(N), [';'], newline,
                  declarations,
                  parse(error).

/**
**/
circuit ::= ['CKTNAME'].
circuit ::= ['cktname'].

begin ::= ['BEGIN_EXTERNAL'].
begin ::= ['BEGIN-EXTERNAL'].

end   ::= ['END_EXTERNAL'].
end   ::= ['END-EXTERNAL'].

/**
**/
declarations ::= declaration, newline, declarations.
declarations ::= [].

declaration ::= ['TYPE', : ],  type(N,T), [';'], update(N,type,T).
declaration ::= [type, : ],    type(N,T), [';'], update(N,type,T).
declaration ::= ['TYPE', : ],  parse(error).
declaration ::= [type, : ],  parse(error).
declaration ::= ['INPUTS', :],   names(Ns), [';'], update(input(Ns)).
declaration ::= ['INPUTS', :], parse(error).
declaration ::= ['OUTPUTS',:],   names(Ns), [';'], update(output(Ns)).
declaration ::= ['OUTPUTS',:], parse(error).
declaration ::= ['IOPUTS', :],   names(Ns), [';'], update(bidir(Ns)).
declaration ::= ['IOPUTS', :], parse(error).
declaration ::= ['INTERNALS',:], names(Ns), [';'], update(internal(Ns)).
declaration ::= ['INTERNALS',:], parse(error).
declaration ::= ['STATES', :, number(Size) ],
                optional_values(InVals),
                more_names(Names), [';'],
                update(states(Names,Size,InVals)).
declaration ::= ['STATES', : ], parse(error).
/**
**/
statements ::= statement, newline, statements.
statements ::= [].
/**
**/

statement ::= ['TYPE', :, Type, '('], name(N), [')'], update(type(Type,Name)).
statement ::= ['TYPE', :], parse(error).
statement ::= ['PWRUP',  :], name(Func), update(powerup(Func)).
statement ::= ['PWRUP',  :], parse(error).
statement ::= ['VIEW',   :], name(Func), update(view(Func)).
statement ::= ['VIEW',   :], parse(error).
statement ::= ['LAITYPE',:], name(LAI),  update(laitype(LAI)).
statement ::= ['LAITYPE',:], parse(error).
statement ::= ['FUNCT', :],  function,
                             boolean_relations, [';'], newline.
statement ::= ['FUNCT', :],  function(_), parse(error).
statement ::= ['FUNCT', :],  parse(error).
/**
**/

statement ::= ['USERDEF', :, quote(Q), ';'], update(quote(Q)).
statement ::= ['USERDEF', :], name(A), [=], name(B), [';'], update(equal(A,B)).
statement ::= ['USERDEF', :], name(_), [=], parse(error).
statement ::= ['USERDEF', :], parse(error).

statement ::= ['CAP', :], names(Ns), [','], min_max(Min, Max), [';'], 
             update(cap(Ns,Min,Max)).

statement ::= ['CAP', :], names(_), [','], parse(error).
statement ::= ['CAP', :], parse(error).

statement ::= ['SIZE', :, Type, '=', F, ';'],
              update(size(Type,F)).
statement ::= ['SIZE', :], parse(error).
/**
**/


statement ::= ['COMMON', :], names(Ns), [';'],
              update(common(Ns)).
statement ::= ['COMMON', :], parse(error).

statement ::= ['BOND_PAD', :], names(Ns), [';'],
              update(bond_pad(Ns)).
statement ::= ['BOND_PAD', :], parse(error).

statement ::= ['PULLUP', :], names(Ns), [',',number(R),';'],
              update(pullup(Ns,R)).
statement ::= ['PULLUP', :], parse(error).

statement ::= ['PULLDOWN', :], names(Ns), [',',number(R),';'],
              update(pulldown(Ns,R)).
statement ::= ['PULLDOWN', :], parse(error).

statement ::= ['MAX_FANOUT', :], names(Ns), [',', number(F), ';'],
              update(max_fanout(Ns,F)).
statement ::= ['MAX_FANOUT', :], parse(error).

statement ::= ['FANOUT_LOAD', :], names(Ns), [',', number(F), ';'],
              update(fanout_load(Ns,F)).
statement ::= ['FANOUT_LOAD', :], parse(error).
/**
**/

statement  ::= ['CLOCK', :], name(N), opt_triggerclock(Type), [';'],
               update(clock(N,Type)).
statement  ::= ['CLOCK', :], name_set(Ns), [';'],
               update(clocks(Ns)).
statement  ::= ['CLOCK', :], parse(error).
statement  ::= ['PRESET',:], name(E), [','], reset_type(T), [';'],
               update(preset_type(S,T)),
               update(preset(S,E)).
statement  ::= ['PRESET',:], parse(error).
statement  ::= ['CLEAR', :], name(N), [','], reset_type(T), [';'],
               update(clear_type(S, T)),
               update(clear(S, N)).
statement  ::= ['CLEAR', :], parse(error).
statement  ::= ['ASYNC', :], name_or_set(Ns), [';'],
               update(async(Ns)).
statement  ::= ['ASYNC', :], parse(error).

statement ::= ['MPWL',:], name(N), [','], min_max(High,Low), [';'],
              update(mpwl(N,High,Low)).
statement ::= ['MPWL',:], parse(error).

statement ::= ['MPWH',:], name(N), [','], min_max(High,Low), [';'],
              update(mpwh(N,High,Low)).
statement ::= ['MPWH',:], parse(error).

statement ::= ['SET', :], names(Ns), [','], name(Trigger), [';'],
              update(set(Ns,Trigger)).
statement ::= ['SET', :], parse(error).

statement ::= ['RESET', :], names(Ns), [','], name(Trigger), [';'],
              update(reset(Ns,Trigger)).
statement ::= ['RESET', :], parse(error).
/**
**/
statement ::= ['SETUP', :], name(D), [','], name(C), [','],
              indent,
                trigger(T), delays(Ds), [';'],
              undent, update(setup(D,C,T,Ds)).
statement ::= ['ASETUP', :], name(D), [','], name(C), [','],
              indent,
                trigger(T), delays(Ds), [';'],
              undent, update(setup(D,C,T,Ds)).
statement ::= ['SETUP', :], parse(error).
statement ::= ['ASETUP', :], parse(error).
statement ::= ['HOLD',  :], name(D),[','], name(C), [','],
              indent,
                trigger(T), delays(Ds), [';'],
              undent, update(hold(D,C,T,Ds)).
statement ::= ['AHOLD',  :], name(D),[','], name(C), [','],
              indent,
                trigger(T), delays(Ds), [';'],
              undent, update(hold(D,C,T,Ds)).
statement ::= ['HOLD',  :], parse(error).
statement ::= ['AHOLD',  :], parse(error).
/**
**/
statement ::= ['TRISTATE_OUTPUT',  :], value_set(Ns), [';'],
              update(tristate_output(Ns)).
statement ::= ['TRISTATE_OUTPUT',  :], parse(error).
statement ::= ['TRISTATE_CONTROL',  :], value_set(Ns), [';'],
              update(tristate_control(Ns)).
statement ::= ['TRISTATE_CONTROL',  :], parse(error).
/**
**/

statement ::= ['SCAN',  :], name(In), [','], name(Out), [','], 
              scan_options(Mode, Trigger, Clocks), [';'],
              update(scan(In,Out,Mode,Trigger,Clocks)).

statement ::= ['SCAN',  :], name(In), [',',','], 
              scan_options(Mode, Trigger, Clocks), [';'],
              update(scan(In,Out,Mode,Trigger,Clocks)).

statement ::= ['SCAN',  :], name(_), [','], name(_), [','], parse(error).
statement ::= ['SCAN',  :], name(_), [','], parse(error).
statement ::= ['SCAN',  :], parse(error).
/**
**/

statement ::= ['DEL', :], name_or_set(Ps),
              update(delay_info(Ps)),
              [','], name(D), [','],
               unateness(Ps,id(Type,D)),
               opt_inversion(Ps),
               opt_trigger(Ps),
             indent,
              model_or_delays(Ps), [';'],
             undent.
/**
**/

statement ::= ['DEL', :],
              name_set(_), [','], name(_), [','],
              unateness(_),
              opt_inversion(_),
              opt_trigger(_),
              parse(error).

statement ::= ['DEL', :], parse(error).
/**
**/

reset_type(low)  ::= ['L'].
reset_type(high) ::= ['H'].

opt_trigger(T) ::= [','], trigger(T).
opt_trigger(notrigger) ::= [].

/**
**/
scan_options(Mode, Trigger, Cs)  ::= name(Mode),  [','],
                                     name(Trigger),
                                     scan_clocks(Cs).

scan_options(_,_,_)  ::= name(_), [','], name(_), parse(error).
scan_options(_,_,_)  ::= name(_), [','], parse(error).

/**
**/
% No Mode and Trigger means there must be a clock.
scan_options(null, null, [C|Cs]) ::= [',',','], scan_clocks([C|Cs]).

scan_options(_,_,_) ::= [',',','], parse(error).

scan_clocks([C|Cs]) ::= [','], name(C), scan_clocks(Cs).
scan_clocks([])     ::= [].

trigger(rise) ::= ['R'].
trigger(fall) ::= ['F'].
trigger(low)  ::= ['L'].
trigger(high) ::= ['H'].

opt_triggerclock(trigger(Type,Clock)) ::= 
      [','], trigger(Type), opt_clock(Clock).

opt_triggerclock(no_trigger) ::=  [].
/**
**/

opt_clock(master)   ::= [',', 'MASTER'].
opt_clock(slave)    ::= [',', 'SLAVE'].
opt_clock(no_clock) ::= [].

/**
**/
unateness(Ps,Id) ::= ['POS_UNATE',','],
                     update(timing_sense(Ps,positive_unate,Id)).
unateness(Ps,Id) ::= ['NEG_UNATE',','],
                     update(timing_sense(Ps,negative_unate:Id)).
unateness(Ps,Id) ::= ['NON_UNATE',','],
                     update(timing_sense(Ps,non_unate,Id)).
unateness(Ps,Id) ::= [], parse,
                     update(timing_sense(Ps,non_unate,Id)).
/**
**/
opt_inversion(Ps) ::= ['~',','], update(invert(Ps)).
opt_inversion(Ps) ::= [],        update(non_invert(Ps)).

/**
**/
model_or_delays(Ps) ::= name(Rise), update(rise_model(Ps,Rise)),
                                    opt_fall_model(Ps,Rise).
model_or_delays(Ps) ::= delays(Ps).

delays([D|Ds])         ::= delay(D), more_delays(Ds).

more_delays([D|Ds])    ::= [','], newline, delay(D), more_delays(Ds).
more_delays([])        ::= [].

delay(delay(R,F))      ::= [',', number(R)], opt_number(R,F).

delay(delay(Type,R,F)) ::= delay_type(Type), ['='],
                           delay_data(R,F).

/**
**/
opt_number(Def,Num)  ::= [',', number(Num)].
opt_number(Def,Def)  ::= [].

opt_fall_model(Ps,_) ::= name(F), update(fall_model(Ps,F)).
opt_fall_model(Ps,R) ::= [],      update(fall_model(Ps,R)).

delay_data(R,F) ::= ['(',number(R), ',', number(F), ')'].
delay_data(R,F) ::= ['('], rise_fall(R,F), [')'].
delay_data(R,F) ::= rise_fall(R,F).

/**
**/
rise_fall(rise(Nom,Min,Max),fall(NomF,MinF,MaxF)) ::=
       [ number(Nom) ],
       opt_rise(Nom,Min,Max),
       opt_fall(Nom,Min,Max,NomF,MinF,MaxF).

opt_rise(  _,Min,Max) ::= [',',number(Min), ',',number(Max)].
opt_rise(Nom,Nom,Nom) ::= [].

opt_fall(_,_,_,Nom,Min,Max) ::=
       [',',number(Nom), ',',number(Min), ',',number(Max)].
opt_fall(Nom,Min,Max,Nom,Min,Max) ::= [].

/**
**/
delay_type('LHZ_SLOPE')      ::= ['LHZ_SLOPE'].
delay_type('ZHL_SLOPE')      ::= ['ZHL_SLOPE'].
delay_type('HL_SLOPE')       ::= ['HL_SLOPE'].
delay_type('LHZ_RESISTANCE') ::= ['LHZ_RESISTANCE'].
delay_type('ZHL_RESISTANCE') ::= ['ZHL_RESISTANCE'].
delay_type('HL_RESISTANCE')  ::= ['HL_RESISTANCE'].
delay_type('ZHL')            ::= ['ZHL'].
delay_type('LHZ')            ::= ['LHZ'].
delay_type('HL')             ::= ['HL'].

/**
**/
function ::= function_type, opt_function_attributes.

function_type ::= ['UNREG'],      update(register).
function_type ::= ['CMB'],        update(combinatorial).
function_type ::= ['COUNTER'],    update(counter).
function_type ::= ['COMPARATOR'], update(comparator).
function_type ::= ['DECODER'],    update(decoder).
function_type ::= ['MEM'],        update(memory).
function_type ::= ['BRAM'],       update(undecoded_memory).
function_type ::= ['MULTIPLEXER'],update(multiplexer).
function_type ::= ['IOBUF'],      update(io_buffer).
function_type ::= ['PARITY'],     update(parity).
function_type ::= ['UNREG'],      update(register).

/**
**/
opt_function_attributes ::= ['('], function_attribute, function_attributes.
opt_function_attributes ::= [].

function_attributes ::= function_attribute, function_attributes.
function_attributes ::= [')'].

function_attribute ::= ['STATIC'],        update(static).
function_attribute ::= ['DYNAMIC'],       update(dynamic).
function_attribute ::= ['POS_LEV_SENSE'], update(positive_sense).
function_attribute ::= ['NEG_LEV_SENSE'], update(negative_sense).
function_attribute ::= ['POS_EDGE_TRIG'],
                       examine(boolean_relation(Pin, _)),
                       update(timing_type(Pin,id(rising_edge,_Cl))).

function_attribute ::= ['NEG_EDGE_TRIG'],
                       update(clocked_on(State, _)),
                       examine(boolean_relation(Pin,id(State))),
                       update(timing_type(Pin, _, falling_edge)).

function_attribute ::= ['SAMPLE'],    update(sample).
function_attribute ::= ['NO_SAMPLE'], update(no_sample).
/**
**/

boolean_relations  ::= boolean_relation1, opt_boolean_relations.

boolean_relation1  ::= [','], newline, name(N), [=], expression(Expr),
                       update(boolean_relation(N, Expr)),
                       opt_boolean_relations.

boolean_relation1 ::= [','], name(_), [=], parse(error).
boolean_relation1 ::= [','], name(_), parse(error).
boolean_relation1 ::= [','], parse(error).
boolean_relation1 ::= [')'],
        parse(warning('WARNING: Extra ) in FUNCT argument ignored')).

/**
**/
opt_boolean_relations ::= boolean_relation1, opt_boolean_relations.
opt_boolean_relations ::= [].

/* 
 * To use the built-in "expression//1", provide rules for
 * operators (including precedence) and primary expressions.
 */

operator(200, xfx, and) ::= ['&'].
operator(300, xfy,  or) ::= ['|'].
operator(100,  fx, not) ::= ['~'].

/**
**/
primary(E)              ::=  ['('], expression(E), [')'].
primary(function(N,As)) ::=  name(N), name_set(As).
primary(V)              ::=  value(V).

value(V)          ::= name(V).
value(number(N))  ::= [number(N)].

/**
**/
optional_values(Vs) ::= value_set(Vs).
optional_values([]) ::= [].

value_set([V|Vs]) ::= ['('], value(V), more_values(Vs), [')'].

more_values([V|Vs]) ::=  [','], value(V), more_values(Vs).
more_values([])     ::=  [].

/**
**/
name(array(Name,A,B)) ::= [ Name , '['], range(A,B), [']'].
name(Name)            ::= [ Name ], { functor(Name,_,0) }.
name(Name)            ::= print, expression(Name).
% name(Name)          ::= print, [ Name ].

names([N|Ns])        ::= name(N), more_names(Ns).

/**
**/
name_or_set([N]) ::= print, expression(N).
name_or_set(Ns)  ::= name_set(Ns).
name_or_set([N]) ::= name(N).

name_set([N|Ns]) ::= ['('], name(N), more_names(Ns), [')'].

more_names([N|Ns]) ::=  [','], name(N), more_names(Ns).
more_names([])     ::=  [].

/**
**/

range(A,B) ::= [number(A)], separator, [number(B)].
range(A,B) ::= name(A), separator, name(B).

separator  ::= [':'].
separator  ::= ['-'].

type(lsl,type) ::=  [ 'LSL' ].
type(  N,   T) ::=  name(T), ['('], name(N), [')'].

min_max(Min,Max) ::= [number(Min), ',', number(Max)].
min_max(Min,Min) ::= [number(Min)].

/**
\end{document}
**/
