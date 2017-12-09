/**
% MOTIVE FORMAT FILE             WARNING: THIS FILE CONTAINS PROPRIETARY
%                                INFORMATION  ABOUT THE MOTIVE LANGUAGE
% Copyright 1992 by INTERGRAPH   AND SHOULD NOT BE CONSIDERED AS DAZIX
% Author:  Peter B. Reintjes     SOURCE CODE.
%

\documentstyle[11pt]{article}
\hfuzz=8pt
\begin{document}
\title{{\LARGE MOTIVE:} A Grammar for MULTIPLEX}
\author{Peter B. Reintjes}
\date{\today}
\maketitle
\vskip 1.0cm
\centerline{\LARGE DRAFT}
\newpage
\section{MOTIVE Lexical Analyzer}
**/

motive lexicon

a whitespace is X if X < 33;

"\+"    is    '+';
"\."    is    '.';
":"     is    ':';
";"     is    ';';
","     is    ',';
"\-"    is    '-';
"\("    is    '(';
"\)"    is    ')';
"="     is    '=';
"\["    is    '[';
"\]"    is    ']';
"\*"    is    '*';
"&"     is    '&';
"\|"    is    '|';
"~"     is    '~';

"DESCRIPTION:.*$"  is desc(String)
                   if (append("DESCRIPTION:",IS,text),
                       append(String,[10],IS)) ;

"SOURCE:.*$"       is source(String)
                   if (append("SOURCE:",IS,text),
                       append(String,[10],IS)) ;

"#"                is Next
                   if (scan_past([10]), continue(Next));

'"[^"]*"'          is quote(double(Q))
                   if append([0'"|Q],[0'"],text);

"'[^']*'"          is quote(single(Q))
                   if append([0''|Q],[0''],text);

"[Oo][0-7]+"       is octal(N)
                   if convert(8,N,text);

"[Xx][0-9A-Fa-f]+" is hex(N)
                   if convert(16,N,text);

"-?[0-9]+"         is number(N)
                   if number_chars(N,text);

"\.[0-9]+([eE][+-]?[0-9]+)?"
                   is number(F)
                   if number_chars(F,[0'0|text]);

"-\.[0-9]+([eE][+-]?[0-9]+)?"
                   is number(F)
                   if ( text = [C|Cs],
                        number_chars(F,[C,0'0|Cs]));

"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?"
                   is number(F)
                   if number_chars(F,text);
"-?[0-9]+\."       is number(F)
                   if (append(text,"0",Num),
                       number_chars(F,Num));

"[0-9]*[a-zA-Z_%][a-zA-Z_0-9<>./$#+-]*"
                   is N
                   if atom_chars(N,text).

/**
\section{MOTIVE Grammar Rules}
**/
motive ::=  file('name.motive', motive), models.

models  ::= model(N), models.
models  ::= [].

model ::= down(Name),
            model(Name),
          up.

model(Name) ::= mheader(Name), body.
/**
**/

mheader(N) ::= mod(N), desc(N), source(N),
               opt_aliases(N), opt_params(N).

mod(Name)    ::= ['MODEL', :, Name], mods(Ns), newline,
                 updates([Name|Ns], name, Name).

mods([Name|Ns]) ::= [',',Name], more_mods(Ns).
mods([])        ::= [].

desc(Name)   ::= [ desc(String) ], newline,
                 update(Name,desc,String).

source(Name) ::= [ source(String) ], newline,
                 update(Name, source, String).

opt_aliases(Name) ::= [ 'ALIAS', :, Alias ],
                      update(Name, alias, Alias),
                      more_aliases(Name), newline,
                      opt_aliases(Name).
opt_aliases(_)    ::= [].

more_aliases(Name) ::=  [',', Alias],
                        update(Name,alias,Alias),
                        more_aliases(Name).
more_aliases(_)    ::=  [].

opt_params(Name)   ::= ['PARAMS',:], pitem(Name),
                       plist(Name), [';'], newline.
opt_params(Name)   ::= [].

pitem(Name)        ::= [Var, '=', Literal],
                       update(Var, literal_param, Literal).
pitem(Name)        ::= [Var, '='], expression(Exp),
                       update(Var, expr_param, Exp).

plist(Name)        ::= [','], pitem(Name), plist(Name).
plist(Name)        ::= [].

/**
\subsection{Pin Definitions}
**/
pindefs  ::= pindef, opt_pindefs.

opt_pindefs ::= pindef, opt_pindefs.
opt_pindefs ::= [].

pindef ::= pinline(Type),
            opt_subpart(Type),
                    opt_sets(Type),
                    piogrp(Type), opt_piogrps(Type),
                    opt_cons(Type),
                   end_pin, newline.

pinline(package([P|Ps],I)) ::= [ 'PINDEF', Package],
                               opt_pkglist(Ps),
                               [ integer(I), 'PINS'], newline.

% Documentation is unclear about the end-pin(def) statement
end_pin  ::= ['END_PINDEF'].
end_pin  ::= ['END_PIN'].

opt_pkglist([P|Ps]) ::= [',', P], opt_pkglist(Ps).
opt_pkglist([])     ::= [].

opt_subpart(Ss) ::= subpart, newline, subpl(Ss).

subpart  ::= ['SUBPARTS'].
subpart  ::= ['SUBPART'].

subpl([S|Ss])  ::= [Sname, =, '['], subpins(S), [']'], newline,
                   subpl(Ss).
subpl([])      ::= [].

piogrp(T)      ::= piot(Type, List), pgrpdefs(Type, List).

opt_piogrps([T|Ts],Info) ::= piogrp(T,Info),
                             opt_piogrps(Ts,Info).
opt_piogrps([],       _) ::= [].

piot(Type,List) ::= piotype(Type), opt_list(List), newline.

piotype(inputs)  ::= [ 'INPUTS' ].
piotype(outputs) ::= [ 'OUTPUTS'].
piotype(bidir)   ::= [ 'BIDIR'  ].
piotype(tristate)::= ['TRISTATE'].
piotype(constant)::= ['CONSTANT'].
piotype(mixed)   ::= [ 'MIXED'  ].
piotype(ioin)    ::= [ 'IOIN'   ].
piotype(ioout)   ::= [ 'IOOUT'  ].
piotype(iobi)    ::= [ 'IOBI'   ].

/**
\subsection{Pin Group Definitions}
**/
pgrpdefs  ::= grpdef, opt_grpdefs.

opt_grpdefs  ::= grpdef, opt_grpdefs.
opt_grpdefs  ::= [].

grpdef  ::= ['GNAME', = ], grppins,
             opt_list, [';'], newline.

grppins([P|Ps]) ::= ['[', P ], opt_gpinlist(Ps), [']'].
grppins([P])    ::= [ P ].

opt_gpinlist([P|Ps]) ::= [',', P], opt_gpinlist(Ps).
opt_gpinlist([])     ::= [].

opt(cap(E))  ::= ['CAP',  = ], expression(real,E).
opt(drvr(D)) ::= ['DRVR', =, D].
opt(rcvr(R)) ::= ['RCVR', =, R].

/**
\subsection{Model Body}
**/

body  ::=  opt_sets(S), pindefs, opt_cons.

opt_sets([set(S,V)|Ss])  ::= set(S,V),  opt_sets(Ss).
opt_sets([cond(C,E)|Ss]) ::= cond(C,E), opt_sets(Ss).
opt_sets([])              ::= [].


set(S, V)   ::= ['SET',S,=],expression(real,Exp),[';'],newline.

cond(if(C,E)) ::= [ 'IF' ],  expression(logical,Exp), newline.
cond(else)    ::= ['ELSE'],  newline.
cond(endif)   ::= ['ENDIF'], newline.

opt_cons([C|Cs]) ::= con(C), opt_cons(Cs).
opt_cons([])     ::= [].

con(C)  ::= delayx(C).
con(C)  ::= sethldx(C).
con(C)  ::= pulsew(C).
con(C)  ::= set(C).
con(C)  ::= skew(C).
con(C)  ::= corr(C).
con(C)  ::= copins(C).
con(C)  ::= conds(C).

delayx(D) ::= delay(D0), derise(D0,D).
delayx(D) ::= delay(D).

sethldx(S) ::= sethld(S0), sh_hold(S0,S).
sethldx(S) ::= sethld(S).

/**
\subsection{Bidirectional}
**/
pg_bi   ::= ['BIDIR'], optionlist, newline,
            grpdef, opt_grpdefs.

grpdef(Name)::= [ Name, = ], grppins,
                opt_grp_options, [';'], newline.

opt_grpdefs([G|Gs]) ::= grpdef(G), opt_grpdefs(Gs).
opt_grpdefs([])     ::= [].

grppins         ::= [ PName, '[' ],
                    gpin(GName), gpinl(GNames), [']'].

gpinl([N|Ns])   ::= [',', N], gpinl(Ns).
gpinl([])       ::= [].

optionlist([O|Os]) ::= [:], bi_option(O),
                       opt_bi_options(Os), [';'].

opt_bi_options  ::= bi_option(O), opt_bi_options(Os).
opt_bi_options  ::= [].

bi_option    ::= ['CAP', = ], expression(real,Exp).
bi_option    ::= ['DRVR', =, Driver ].
bi_option    ::= ['RCVR', =, Receiver].
/**
\subsection{Constant}
**/
pg_con  ::= constant, newline, grpdef, opt_grpdefs.

constant ::= ['CONSTANT'].
constant ::= ['CONST'].

/**
\subsection{Correlation Coefficients}
**/
corr   ::= ['CORR',float(Direct), float(Cross)], newline,
           update(Direct,correlation,Cross).

cordef ::= ['CORDEF',float(Direct), float(Cross)], newline,
            update(Direct,correlation_def,Cross).

co_metal ::= ['CO_METAL'], drvr(ID), 
             ['PF_PER_FANOUT', :, number(Incr),
              'CMAX', :, number(Limit) ],
             update(ID, co_metal, metal(pf(Incr),pf(Limit))).

metal ::= ['METAL'], drvr(ID), 
           ['PF_PER_FANOUT', :, number(Incr),
            'CMAX', :, number(Limit) ],
           update(ID, metal, metal(pf(Incr),pf(Limit))).

% This says that grp(One) cap is to be added to grp(Two)
% A transitive relation requires two such statements

co_pins  ::= grp(One), grp(Two), update(One,copins,Two).

defdriver  ::= ['DEFDRIVER', Otype,
                'VH', :], expression(real,Vh),
               ['VM', :], expression(real,Vm),
               ['VL', :], expression(real,Vl),
               ['RH', :], expression(real,Rh),
               ['RL', :], expression(real,Rl),
               ['CSPEC', :], expression(real,Cap).
defthr     ::= ['DEFTHR', Itype,
                'VTH', :], expression(real,High),
               ['VTL', :], expression(real,Low).

/**
\subsection{Delays}
**/
delay ::= ['DELAY'], intype(T), grp(In),op(O), grp(Out),
          [Minlh, Typlh, Maxlh, Minhl, Typhl, Maxhl],
          update(T,delay,
           data(In,O,Out, Minlh, Typlh, Maxlh,
                          Minhl, Typhl, Maxhl)).

delay ::= ['DELAYV'], intype(T), grp(In),op(O), grp(Out),
          [Minlh, Typlh, Maxlh, Minhl, Typhl, Maxhl],
          update(T,delay,
           data(In, O, Out, Minlh, Typlh, Maxlh,
                            Minhl, Typhl, Maxhl)).

delay ::= ['DELAYI'], intype(T), grp(In),op(O), grp(Out),
          [Minlh, Typlh, Maxlh, Minhl, Typhl, Maxhl],
          update(T,delay,
           data(In, O, Out, Minlh, Typlh, Maxlh,
                            Minhl, Typhl, Maxhl)).

intype(high_set)    ::= ['S'].
intype(high_clear)  ::= ['C'].
intype(low_set)     ::= [s].
intype(low_clear)   ::= [c].
intype(invert)      ::= ['I'].
intype(non_invert)  ::= ['N'].
intype(cond_invert) ::= ['E'].
intype(high_gated_latch) ::= ['GL'].
intype(low_gated_latch)  ::= ['gL'].
intype(invert_latch)     ::= ['IL'].
intype(non_invert_latch) ::= ['NL'].
intype(cond_invert_latch)::= ['EL'].
intype(rising)     ::= ['R'].
intype(falling)    ::= ['F'].
intype(noi)        ::= ['NOI'].
intype(nai)        ::= ['NAI'].
intype(non)        ::= ['NON'].
intype(nan)        ::= ['NAN'].
intype(ioi)        ::= ['IOI'].
intype(iai)        ::= ['IAI'].
intype(ion)        ::= ['ION'].
intype(ian)        ::= ['IAN'].

/**
\subsection{Rise-time Derating}
**/

derise(D)  ::= ['DERISE', :], d1(D), d2(D), d3(D), d4(D),
               newline.

d1(Delay) ::= ['DR','(','DR',')',:], expression(real, Expr),
               update(Delay, drdr, Expr).
d2(Delay) ::= ['DF','(','DR',')',:], expression(real, Expr),
               update(Delay, dfdr, Expr).
d3(Delay) ::= ['DR','(','DF',')',:], expression(real, Expr),
               update(Delay, drdf, Expr).
d4(Delay) ::= ['DF','(','DF',')',:], expression(real, Expr),
               update(Delay, dfdf, Expr).
/**
\subsection{I/O Pin Statements}
**/

pg_in    ::=   input,    opt_optlist, newline, grpdefs.
pg_out   ::= ['OUTPUT'], opt_optlist, newline, grpdefs.

input    ::= ['INPUT'].
input    ::= ['INPUTS'].

pg_iobi  ::= ['IOBI'],  newline, grpdefs.
pg_ioout ::= ['IOOUT'], newline, grpdefs.

/**
\subsection{Knee}
**/
knee  ::= ['KNEE'], drvr(ID), [ 'CAP',:, number(Cap),
                                'RH', :, number(RH),
                                'RL', :, number(RL)  ],
          update(ID,knee,knee(pf(Cap),ps(RH),ps(RL))).

min_max  ::= ['MIN_MAX'], drvr(ID), [number(Spread)],
             update(ID, min_max_spread, Spread).

/**
\subsection{Mixed Pins (previously defined)}
**/

pg_mix  ::= ['MIXED'], newline, grpdefs.

pulsewidth  ::= ['PULSEWIDTH', Group, 'LOW'],
                expression(real, Expr),
                update(Group, min_low_pulsewidth, Expr).
pulsewidth  ::= ['PULSEWIDTH', Group, 'HIGH'],
                expression(real, Expr),
                update(Group, min_high_pulsewidth, Expr).

risefall ::=  ['RISEFALL', Otype, 'TR',:, number(TRise),
                                  'TF',:, number(TFall),
                                  'DR',:, number(DRise),
                                  'DF',:, number(DFall) ],
               update(Otype,trise_delay,TRise),
               update(Otype,tfall_delay,TFall),
               update(Otype,drise_delay,DRise),
               update(Otype,dfall_delay,DFall).


series_r ::= ['SERIES_R', Otype, number(Res)],
               update(Otype, series_resistance, Res).

set      ::= ['SET', Var, = ], expression(real, Expr),
             update(Var, set, Expr).

sethld_wrt ::= ['SETHLD', 'WRT', Edge3], grpl(G3),
               grpl(G1), [Op], grpl(G2), [ A, B, C, D ],
               newline.

sethld     ::= ['SETHLD'], 
               grpl(G1), [Op], grpl(G2), [ A, B, C, D ],
               newline.

/**
The group-list must have the same number of
members as grp1 in the SETHLD statement
**/

sh_load    ::= ['SH_LOAD', Polarity], pingrp(Ps),
               update(Ps, polarity, Polarity).

skew    ::= ['SKEW', 'PIN', Pname1, 'TO', 'PIN', Pname2 ],
            expression(real, Skew_LH),
            expression(real, Skew_HL), newline,
            update(Pname1,low_to_high_skew(Pname2),Skew_LH),
            update(Pname1,high_to_low_skew(Pname2),Skew_HL).

tristate  ::= ['TRISTATE'], optionlist(Os), newline,
              grpdefs(Ts),
              update(Ts,tristate,Os).

/**
\subsection{Expressions}
To use the built-in {\tt expression//1}, provide rules for
operators (including precedence) and primary expressions.
**/

operator(300,xfy, plus)  ::= [+].
operator(300,xfy, or)    ::= ['|'].
operator(305,xfy, xor)   ::= [^].
operator(200,xfy, times) ::= [*].
operator(200,xfy, div)   ::= [/].
operator(200,xfy, and)   ::= [&].

/**
**/
primary(Expr)       ::=  ['('], expression(Expr), [')'].
primary(float(F))   ::=  [ float(F) ].
primary(integer(F)) ::=  [ integer(F) ].
primary(id(A))      ::=  label(A).

/**
\end{document}
**/
