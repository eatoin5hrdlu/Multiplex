/**
% Verilog FORMAT FILE
%
% Copyright 1991 by INTERGRAPH 
% Author:  Peter B. Reintjes

\chapter{Verilog: A Grammar for MULTIPLEX}
\section{Verilog Lexical Analyzer}
**/
verilog lexicon

"\." ; ";"  ; "'"  ; ","  ; ":" ; "!" ; "\*" ;
"\+" ; "\-" ; "\(" ; "\)" ; "{" ; "}" ;
'"[^"]*"' is  quote(double(Chs))
          if  append([0'"|Chs],[0'"],text);

"/*"      is  Result
          if  (scan_past("*/"), continue(Result));

"[+-]?[0-9]+" is integer(N)
              if (text(T), name(N,T));
"[+-]?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?"
              is float(F)
              if (text(T),float_chars(F,T));
"-\.[0-9]+([eE][+-]?[0-9]+)?"
              is float(F)
              if (text(T),float_chars(F,T));
"\.[0-9]+([eE][+-]?[0-9]+)?"
              is float(F)
              if (text(T),float_chars(F,T));
"[a-zA-Z_][a-zA-Z0-9<>_]*"
              is N
              if (text(T),name(N,T)) ;
"[ \t\n]+"    is N 
              if switch(verilog,N).

/**
\section{The Verilog Grammar}
A Verilog program consists  of one or more modules, macro modules,
or primitives. This section defines their syntax. Most of these
rules are self-documented.
**/
verilog ::= file('name.vlg', verilog), source_text(Ds).
/**
\subsubsection{Rule 1}
**/
source_text(Ds)::= 
    opt_list_of_descriptions(Ds).
/**
**/
opt_list_of_descriptions([D|Ds]) ::=
    description(D),
    opt_list_of_descriptions(Ds).
opt_list_of_descriptions([]) ::= [].
/**
\subsubsection{Rule 2}
A description can be a module, or a primitive.
**/
description(M) ::= module(M),!.
description(P) ::= primitive(P).
/**
\subsubsection{Rule 3}
A module is a module or a macromodule
**/
module(module(Mname,Ports,Items)) ::=
    [module],
    identifier(Mname), 
    opt_ports_list(Ports),
    [';'],
    opt_list_of_module_items(Items),
    [endmodule].
module(macromodule(Mname,Ports,Items)) ::=
    [macromodule],
    identifier(Mname), 
    opt_ports_list(Ports),
    [';'],
    opt_list_of_module_items(Items),
    [endmodule].
/**
\subsubsection{Rule 4}
**/
opt_ports_list(Ps) ::= ports_list(Ps),!.
opt_ports_list([]) ::= [].
/**
Here is an instance of left-factoring. Additional
ports in the ports\_list can be detected by a ",".
On the other hand end of the ports\_list can be
detected by a ")". Hence we factor the {\tt ports\_list//1}
rule into two separate rules.
**/
ports_list([P|Ps]) ::=
      ['('],
      port(P), 
      additional_ports_list(Ps).
/**
**/
additional_ports_list([]) ::= [')'],!.
additional_ports_list([Port|Ps])::=
    [','],
    port(Port), 
    additional_ports_list(Ps).
/**
**/
/**
\subsubsection{Rule 5}
**/
port(port(Pname,Pexpr)) ::= 
    ['.'],
    identifier(Pname),
    opt_port_expression(Pexpr),
    [')'].
port(port(Pexpr)) ::= opt_port_expression(Pexpr).
/**
\subsubsection{Rule 6}
**/
opt_port_expression([Ref|Refs]) ::= 
    ['{'],!,
    port_reference(Ref),
    opt_port_reference_list(Refs).
opt_port_expression(Ref) ::= port_reference(Ref).
opt_port_expression(null) ::= [].
/**
\subsubsection{Rule 7}
Left factoring done again.
**/
opt_port_reference_list([]) ::= ['}'],!.
opt_port_reference_list([Ref|Refs]) ::=
    [','],
    port_reference(Ref),
    opt_port_reference_list(Refs).
/**
**/
port_reference(port_ref(V,Expr)) ::= 
    identifier(V),
    opt_select_expression(Expr).
/**
**/
opt_select_expression(bit_selexpr(Expr)) ::=
    ['['], !,
    constant_expression(Expr),
    [']'].
opt_select_expression(part_selexpr(E1,E2)) ::=
    ['['], !,
    constant_expression(E1),
    [':'],
    constant_expression(E2),
    [']'].
opt_select_expression(null) ::= [].

/**
\subsubsection{Rule 8}
**/
opt_list_of_module_items([MI|MIs]) ::=
    	module_item(MI), !,
    opt_list_of_module_items(MIs).
opt_list_of_module_items([]) ::= [].
/**
**/
module_item(Param_Ds) ::=  parameter_decln(Param_Ds).
module_item(Input_Ds) ::= input_decln(Input_Ds).
module_item(Output_Ds) ::= output_decln(Output_Ds).
module_item(Inout_Ds) ::= inout_decln(Inout_Ds).
module_item(Net_Ds) ::=   net_decln(Net_Ds).
module_item(Reg_Ds) ::= reg_decln(Reg_Ds).
module_item(Time_Ds) ::= time_decln(Time_Ds).
module_item(Int_Ds) ::=  integer_decln(Int_Ds).
module_item(Real_Ds) ::= real_decln(Real_Ds).
module_item(Event_Ds) ::= event_decln(Event_Ds).
module_item(Gate_Is) ::= gate_instantiation(Gate_Is).
module_item(P) ::=  parameter_override(P). 
module_item(Cont_assgn) ::= continous_assign(Cont_assgn).	
module_item(Sp_block) ::= specify_block(Sp_block).
module_item(Stmnt)  ::= initial_statement(Stmnt).
module_item(Stmnt) ::= 	always_statement(Stmnt).
module_item(Task) ::= task(Task).
module_item(Function) ::= function(Function).
module_item(MPI) ::= module_instantiation(MPI).
module_item(MPI) ::= primitive_instantiation(MPI).

/**
\subsubsection{Rule 9}
**/
primitive(primitive(Pname,[Var|Vars],Decl,Defn)) ::=
    [primitive],
    identifier(Pname),
    ['('],
    identifier(Var),
    opt_list_of_prim_vars(Vars),
    list_of_prim_declarations(Decl),
    table_definition(Defn),
    [endprimitive].
/**
**/
opt_list_of_prim_vars([]) ::= [')'], !,[';'].
opt_list_of_prim_vars([V|Vs]) ::= 
    [','],
    identifier(V), 
    opt_list_of_prim_vars(Vs).
/**
\subsubsection{Rule 10}
**/
list_of_prim_declarations([D|Ds]) ::=
    prim_declaration(D),
    opt_list_of_prim_declarations(Ds).

opt_list_of_prim_declarations([D|Ds]) ::=
    prim_declaration(D),
    opt_list_of_prim_declarations(Ds).
opt_list_of_prim_declarations([]) ::= [].

prim_declaration(P) ::= output_decln(P).
prim_declaration(P) ::= input_decln(P).
prim_declaration(P) ::= reg_decln(P).
/**
\subsubsection{Rule 11}
**/
table_definition(table_defn(Entries)) ::=
    [table],
    table_entries(Entries),
    [endtable].
/**
\subsubsection{Rule 12}
**/
table_entries([E|Es]) ::=
    combinational_entry(E),
    opt_list_of_combinational_entries(Es).
table_entries([E|Es]) ::=
    sequential_entry(E),
    opt_list_of_sequential_entries(Es).

opt_list_of_combinational_entries([E|Es]) ::=
    combinational_entry(E),!,
    opt_list_of_combinational_entries(Es).
opt_list_of_combinational_entries([]) ::= [].

opt_list_of_sequential_entries([E|Es]) ::=
    sequential_entry(E),!,
    opt_list_of_sequential_entries(Es).
opt_list_of_sequential_entries([]) ::= [].
/**
\subsubsection{Rule 13}
**/
combinational_entry(comb_entry(lil(Lis),O)) ::=
    level_input_list(Lis-[]),
    [':'],
    [Mark],
    {output_symbol(Mark,O)},
    [';'].
combinational_entry(_,Tlist,_) :-
    length(Tlist,L),
    assertz(syntax_error(L,"Error in Primitive (Combinational) Table Entry")),
    !,fail.

/**
\subsubsection{Rule 14}
**/
sequential_entry(seq_entry(Is,St,Nxtst)) ::=
    input_list(Is),
    [':'],
    [Mark],
    state(Mark,St),
    [':'],
    [Mark1],
    next_state(Mark1,Nxtst),
    [';'].
sequential_entry(_,Tlist,_) :-
    length(Tlist,L),
    assertz(syntax_error(L,"Error in Primitive Sequential) Table Entry")),
    !,fail.
/**
\subsubsection{Rule 15}
**/
input_list(lil(Lis)) ::=
    level_input_list(Lis-[]).
input_list(Eis) ::=
    edge_input_list(Eis).
/**
\subsubsection{Rule 16}
**/
level_input_list(Lilfr-Lilbk) ::= 
    [Mark],
    {collect_test_lsyms(Mark,Lsfr-Lsbk)},!,
    opt_level_input_list(Restfr-Restbk),
    {append_dl(Lsfr-Lsbk,Restfr-Restbk,Lilfr-Lilbk)}.

opt_level_input_list(Lilfr-Lilbk) ::=
    [Mark],
    {collect_test_lsyms(Mark,Lsfr-Lsbk)},!,
    opt_level_input_list(Restfr-Restbk),
    {append_dl(Lsfr-Lsbk,Restfr-Restbk,Lilfr-Lilbk)}.
opt_level_input_list(Z-Z) ::= [].
/**
\subsubsection{Rule 17}
**/
edge_input_list(eil(lil(Is1),Edge,lil(Is2))) ::=
    opt_level_input_list(Is1-[]),
    edge(Edge),
    opt_level_input_list(Is2-[]).
/**
\subsubsection{Rule 18}
**/
edge(edge(A,B)) ::=
    ['('],
    level_input_list([A,B]-[]),
    [')'].
%
% A look-ahead token(Mark) is used here
%
edge(edge(Esym)) ::= 
    [Mark],
    edge(Mark,Esym).

/**
\subsubsection{Rule 19}
This shows that a state symbol can be returned as a number token
({\tt number([1])}) or an identifier token ({\tt x}).
**/
state(number([Stc]),Sta) ::= {level_symbol(Stc,Sta)},!.
state(St_atom,A) ::= 
    {name(St_atom,[C]),
    level_symbol(C,A)},!.
/**
\subsubsection{Rule 20}
**/
next_state('-','-') ::= [],!.
next_state(number([C]),A) ::= !, [], {o_symbol(C,A)}.
next_state(St_atom,A) ::= [],
     {name(St_atom,[C]),
    o_symbol(C,A)}.

/**
\subsubsection{Rule 21}
**/
output_symbol(number([Osymb]),Atom) :-
    o_symbol(Osymb,Atom),!.
output_symbol(Osym_atom,Atom) :-
    name(Osym_atom,[C]),
    o_symbol(C,Atom).
/**
**/
o_symbol(0'0,0).
o_symbol(0'1,1).
o_symbol(0'x,x).
o_symbol(0'X,'X').

/**
\subsubsection{Rule 22}
Level Symbols are parsed by the following prolog predicates that
split the input token (number or identifier) into individual
characters and then verifying that they are indeed Level Symbols.
**/
%
% collect_test_lsysms generates a list of characters
%  that need to be verified as level symbols.
%  The first clause of this predicate generates the
%    characters from a list of digits  (number([Ch|Chs]))
% The second clause generates the list by breaking an atom into a Chlist.
%
collect_test_lsyms(number(Chs),LFr-LBk) :- 
    !,level_symbol_test(Chs,LFr-LBk).
collect_test_lsyms(Atom,LFr-LBk) :- 
    name(Atom,Chlist),
    level_symbol_test(Chlist,LFr-LBk).
%
% level_symbol_test verifies list elements (characters) are level_symbols
%
level_symbol_test([],Z-Z) :- !.
level_symbol_test([Ch|Chlist],[Atom|Rest]-Bk) :- 
    level_symbol(Ch,Atom),
    !,
    level_symbol_test(Chlist,Rest-Bk).

level_symbol(0'0,0).
level_symbol(0'1,1).
level_symbol(0'x,x).
level_symbol(0'X,'X').
level_symbol(0'b,b).
level_symbol(0'B,'B').
level_symbol(0'?,'?').
/**
\subsubsection{Rule 23}
 The edge symbols appear individually. and are thus returned
 as separate tokens (atoms)
**/
edge(r  ,r) ::= [].
edge('R','R') ::= [].
edge(f  ,f) ::= [].
edge('F','F') ::= [].
edge(p  ,p) ::= [].
edge('P','P') ::= [].
edge(n,  n) ::= [].
edge('N','N') ::= [].
edge('*','*') ::= [].

/**
\subsubsection{Rule 24}
**/
task(task(Tname,Ds,Stmt)) ::= 
    [task],
    identifier(Tname),
    [';'],
    opt_tf_declarations(Ds),
    statement_or_null(Stmt),
    [endtask].
/**
\subsubsection{Rule 25}
**/
function(function(Range,Name,Ds,Stmt)) ::= 
    [function],
    opt_range_or_type(Range),
    identifier(Name),
    [';'],
    tf_declarations(Ds),
    statement_or_null(Stmt),
    [endfunction].

/**
\subsubsection{Rule 26}
**/
opt_range_or_type(integer) ::= [integer],!.
opt_range_or_type(real) ::= [real],!.
opt_range_or_type(R) ::= range(R).
opt_range_or_type(null) ::= [].

/**
\subsubsection{Rule 27}
**/
tf_declarations([D|Ds]) ::=
    tf_declaration(D),
    opt_tf_declarations(Ds).

opt_tf_declarations([D|Ds]) ::=
    tf_declaration(D),
    opt_tf_declarations(Ds).
opt_tf_declarations([]) ::= [].

tf_declaration(D) ::=	parameter_decln(D).
tf_declaration(D) ::=	input_decln(D).
tf_declaration(D) ::=	output_decln(D).
tf_declaration(D) ::= inout_decln(D).
tf_declaration(D) ::=	reg_decln(D).
tf_declaration(D) ::= time_decln(D).
tf_declaration(D) ::=	integer_decln(D).
tf_declaration(D) ::= 	real_decln(D).
tf_declaration(D) ::=	event_decln(D).
/**
\subsection{Declarations}
This section describes the grammar rules for all
the declarations.

\subsubsection{Rule 28}
The Thomas-Moorby BNF, says that no range declarations
are permitted for parameter declarations, while
the BNF in the Verilog Reference Manual does permit optional
range declarations. Several Verilog models in the example
files  declare ranges for parameters.
So we  decided to keep them.)
**/
parameter_decln(parameter(Range,Assigns)) ::=
    [parameter],
    opt_range(Range),
    list_of_assignments(Assigns).
/**

\subsubsection{Rule 29}
**/
input_decln(input(Range,Vars)) ::=
    [input],
    opt_range(Range), 
    list_of_variables(Vars).
    
/**

\subsubsection{Rule 30}
**/
output_decln(output(Range,Vars))  ::=
    [output],
    opt_range(Range),
    list_of_variables(Vars).
/**

\subsubsection{Rule 31}
**/
inout_decln(inout(Range,Vars))  ::= 
    [inout],
    opt_range(Range),
    list_of_variables(Vars).
/**

\subsubsection{Rule 32}
**/
net_decln(net_decl(trireg,Cs,Exp_range,Delay,Vars)) ::=
    [trireg], !,
 	opt_charge_strength(Cs),
 	opt_expand_range(Exp_range),
 	opt_delay(Delay),
    list_of_variables(Vars).

net_decln(net_decl(Type,Exp_range,Delay,Vars)) ::=
    net_type(Type),
 	opt_expand_range(Exp_range),
 	opt_delay(Delay),
    list_of_variables(Vars).

net_decln(net_decl(Type,Ds,Exp_range,Delay,Asmts)) ::=
    net_type(Type),
 	opt_drive_strength(Ds),
 	opt_expand_range(Exp_range),
 	opt_delay(Delay),
    list_of_assignments(Asmts).
/**
**/
net_type(wire) ::= [wire],!.
net_type(tri) ::= [tri],!.
net_type(tri1) ::= [tri1],!.
net_type(supply0) ::= [supply0],!.
net_type(wand) ::= [wand],!.
net_type(triand) ::= [triand],!.
net_type(tri0) ::= [tri0],!.
net_type(supply1) ::= [supply1],!.
net_type(wor) ::= [wor],!.
net_type(trior) ::= [trior].

/**
\subsubsection{Rule 33}
**/
opt_expand_range(scalared(Rexp)) ::=
    [scalared],!,
    range(Rexp).
opt_expand_range(vectored(Rexp)) ::=
    [vectored],!,
    range(Rexp).
opt_expand_range(Rexp) ::=
    range(Rexp).
opt_expand_range(null) ::= [].
/**

\subsubsection{Rule 34}
**/
reg_decln(reg(Range,Rvs))  ::= 
    [reg],
    opt_range(Range),
    list_of_reg_variables(Rvs).
/**

\subsubsection{Rule 35}
 The Thomas-Moorby BNF disallows optional ranges for
time and integer declaration while they are permitted in the
Gateway Verilog-XL Reference manual. We do not show them here.
**/
time_decln(time(Rvs))  ::=
    [time],
    list_of_reg_variables(Rvs).
/**

\subsubsection{Rule 36}
**/
integer_decln(integer(Rvs))  ::= 
    [integer],
    list_of_reg_variables(Rvs).
/**
\subsubsection{Rule 37}
**/
real_decln(real(Rvs))  ::= 
    [real],
    list_of_variables(Rvs).

/**
\subsubsection{Rule 38}
**/
event_decln(event_dec([E|Es])) ::=
    [event],
    identifier(E),
    opt_additional_events(Es).

opt_additional_events([])  ::= [';'],!.
opt_additional_events([E|Es])  ::= 
    [','],
    identifier(E),
    opt_additional_events(Es).

/**
\subsubsection{Rule 39}
**/
continous_assign(cont_assign(Strength,Delay,As)) ::=
    [assign],
    opt_drive_strength(Strength),
    opt_delay(Delay),
    list_of_assignments(As).
/**
\subsubsection{Rule 40}
**/
parameter_override(param_override(Ps)) ::=
    [defparam],
    list_of_assignments(Ps).
/**

\subsubsection{Rule 41}
**/
list_of_variables([V|Vs]) ::= 
    identifier(V),
    opt_list_of_variables(Vs).
%
% Syntax error Marker: Error in Declaration
%
list_of_variables(_,Tlist,_) :-
    length(Tlist,L),
    assertz(syntax_error(L,"Error in Variable Declaration")),
    !,fail.

/**
**/
opt_list_of_variables([])     ::= [';'],!.
opt_list_of_variables([V|Vs]) ::= 
    [','],
    identifier(V),
    opt_list_of_variables(Vs).
/**
\subsubsection{Rule 42}
**/
list_of_reg_variables([Rv|Rvs]) ::= 
    reg_variable(Rv),
    opt_list_of_reg_variables(Rvs).
list_of_reg_variables(_,Tlist,_) :-
    length(Tlist,L),
    assertz(syntax_error(L,"Error in Register Variable  decln")),
    !,fail.
/**
**/
opt_list_of_reg_variables([])     ::= [';'],!.
opt_list_of_reg_variables([Rv|Rvs]) ::= 
    [','],
    reg_variable(Rv),
    opt_list_of_reg_variables(Rvs).

/**
\subsubsection{Rule 43}
**/
reg_variable(Rv) ::=
    identifier(Id),
    opt_array_id(Id,Rv).
/**
Left Factoring
**/
opt_array_id(Id,array(Id,E1,E2)) ::= 
    ['['], !,
    constant_expression(E1), 
    [':'],
    constant_expression(E2),
    [']'].
opt_array_id(Id,Id) ::= [].

/**
\subsubsection{Rule 44}
**/
opt_charge_strength(small) ::=	['('], [small], !, [')'].
opt_charge_strength(medium) ::=	['('], [medium], !, [')'].
opt_charge_strength(large) ::=	['('], !, [large], [')'].
opt_charge_strength(null) ::= [].
/**
\subsubsection{Rule 45}
**/
opt_drive_strength(drive_str(S0,S1)) ::=
    ['('],  
    strength0(S0),
    [','],
    strength1(S1),
    [')'].
opt_drive_strength(drive_str(S1,S0)) ::=
    ['('], 
    strength1(S1),
    [','],
    strength0(S0),
    [')'].
opt_drive_strength(null) ::= [].
/**
\subsubsection{Rule 46}
**/
strength0(supply0) ::= [supply0], !.
strength0(strong0) ::= [strong0], !.
strength0(pull0) ::= [pull0], !.
strength0(weak0) ::= [weak0], !.
strength0(highz0) ::= [highz0].
/**
\subsubsection{Rule 47}
**/
strength1(supply1) ::= [supply1], ! .
strength1(strong1) ::= [strong1], !.
strength1(pull1) ::= [pull1], !.
strength1(weak1) ::= [weak1], !.
strength1(highz1) ::= [highz1].
/**

\subsubsection{Rule 48}
**/
range(range(E1,E2)) ::= 
    ['['],
    constant_expression(E1), 
    [':'],
    constant_expression(E2),
    [']'].

/**
**/
opt_range(R) ::= range(R).
opt_range(null) ::= [].
/**
\subsubsection{Rule 49}
**/
list_of_assignments([A|As]) ::=
    assignment(A),
    opt_list_of_assignments(As).
list_of_assignments(_,Tlist,_) :-
    length(Tlist,L),
    assertz(syntax_error(L,"Error in Assignment Statement")),
    !,fail.

/**
**/
opt_list_of_assignments([A|As]) ::=
    [','],!,
    assignment(A),
    opt_list_of_assignments(As).
opt_list_of_assignments([]) ::= [';'],!.
opt_list_of_assignments(_,Tlist,_) :-
    length(Tlist,L),
    assertz(syntax_error(L,"Error in Assignment Statement")),
    !,fail.


/**
\subsection{Primitive Instances}
\subsubsection{Rule 50}
**/
gate_instantiation(gate_instan(Type,Ds,Delay,GIs)) ::=
    gate_type(Type),
    opt_drive_strength(Ds),
    opt_delay(Delay),
    gate_instance_list(GIs).
/**
**/
gate_type(and) ::= [and], !.
gate_type(nand) ::= [nand], !.
gate_type(or) ::= [or], !.
gate_type(nor) ::= [nor], !.
gate_type(xor) ::= [xor], !.
gate_type(xnor) ::= [xnor], !.
gate_type(buf) ::= [buf], !.
gate_type(bufif0) ::= [bufif0], !.
gate_type(bufif1) ::= [bufif1], !.
gate_type(not) ::= [not], !.
gate_type(notif0) ::= [notif0], !.
gate_type(notif1) ::= [notif1], !.
gate_type(pulldown) ::= [pulldown], !.
gate_type(pullup) ::= [pullup], !.
gate_type(nmos) ::= [nmos], !.
gate_type(rnmos) ::= [rnmos], !.
gate_type(pmos) ::= [pmos], !.
gate_type(rpmos) ::= [rpmos], !.
gate_type(cmos) ::= [cmos], !.
gate_type(rcmos) ::= [rcmos], !.
gate_type(tran) ::= [tran], !.
gate_type(rtran) ::= [rtran], !.
gate_type(tranif0) ::= [tranif0], !.
gate_type(tranif1) ::= [tranif1], !.
gate_type(rtranif0) ::= [rtranif0].
gate_type(rtranif1) ::= [rtranif1].

/**
\subsubsection{Rule 51}
**/
gate_instance_list([I|Is]) ::=
    gate_instance(I),!,
    opt_gate_instance_list(Is).

opt_gate_instance_list([]) ::= [';'],!.
opt_gate_instance_list([I|Is]) ::=
    [','],
    gate_instance(I),!,
    opt_gate_instance_list(Is).
opt_gate_instance_list(_,Tlist,_) :-
    length(Tlist,L),
    assertz(syntax_error(L,"Error in Gate Instance description")),
    !,fail.


/**
**/
gate_instance(gi(Name,Ts)) ::=
    opt_identifier(Name),
    ['('],
    list_of_terminals(Ts).
/**
\subsubsection{Rule 52}
**/
primitive_instantiation(prim_instan(Name,Drs,Delay,Ps)) ::=
    identifier(Name),
    opt_drive_strength(Drs),
    opt_delay(Delay),
    primitive_instance_list(Ps).

primitive_instance_list([I|Is]) ::=
    primitive_instance(I),
    opt_primitive_instance_list(Is).

opt_primitive_instance_list([]) ::= [';'],!.
opt_primitive_instance_list([I|Is]) ::=
    [','],
    primitive_instance(I),
    opt_primitive_instance_list(Is).
/**
\subsubsection{Rule 53}
**/
primitive_instance(pi(Name,Ts)) ::=
    opt_identifier(Name),
    ['('],
    list_of_terminals(Ts).
/**
\subsubsection{Rule 54}
**/
list_of_terminals([T|Ts]) ::=
    terminal(T),
    opt_list_of_terminals(Ts).

opt_list_of_terminals([]) ::= [')'],!.
opt_list_of_terminals([T|Ts]) ::= 
    [','],
    terminal(T),
    opt_list_of_terminals(Ts).

terminal(terminal(E)) ::= expression(E).

/**
\subsection{Module Instantiations}
\subsubsection{Rule 55}
**/
module_instantiation(mod_instan(Name,PVa,Ms)) ::=
    identifier(Name),
    opt_parameter_value_assgnmt(PVa),
    module_instance_list(Ms).
/**
\subsubsection{Rule 56}
**/
opt_parameter_value_assgnmt(param_val_assgn([E|Es])) ::=
    ['#'], !,
    ['('],
    expression(E),
    opt_list_of_expressions(Es).
opt_parameter_value_assgnmt(null) ::= [].

opt_list_of_expressions([]) ::= [')'],!.
opt_list_of_expressions([E|Es]) ::=
    [','],
    expression(E),	
    opt_list_of_expressions(Es).

/**
\subsubsection{Rule 57}
**/
module_instance_list([I|Is]) ::=
    module_instance(I),!,
    opt_module_instance_list(Is).

opt_module_instance_list([]) ::= [';'],!.
opt_module_instance_list([I|Is]) ::=
    [','],
    module_instance(I),
    opt_module_instance_list(Is).
opt_module_instance_list(_,Tok_list,_) :- 
    length(Tok_list,L),
    assertz(syntax_error(L,"Error in Module Instance Description")),
    !,fail.


module_instance(mi(Name,MCs)) ::=
    identifier(Name),
    ['('],
    opt_list_of_module_conections(MCs),
    [')'].
/**
\subsubsection{Rule 58}
**/
opt_list_of_module_conections(MPCs) ::=
    list_of_module_port_conections(MPCs).
opt_list_of_module_conections(NPCs) ::=
    list_of_named_port_conections(NPCs).
opt_list_of_module_conections(null) ::= [].

list_of_module_port_conections([MPC|MPCs]) ::=
    module_port_connection(MPC),
    opt_list_of_module_port_conections(MPCs).
list_of_named_port_conections([NPC|NPCs]) ::=
    named_port_connection(NPC),
    opt_list_of_named_port_conections(NPCs).
/**
\subsubsection{Rule 59}
**/
opt_list_of_module_port_conections([MPC|MPCs]) ::=
    [','], !,
    module_port_connection(MPC),
    opt_list_of_module_port_conections(MPCs).
opt_list_of_module_port_conections([]) ::= [].
/**
**/
module_port_connection(mpc(E)) ::= expression(E).
module_port_connection(null) ::= [].

/**
\subsubsection{Rule 60}
**/
opt_list_of_named_port_conections([NPC|NPCs]) ::=
    [','],
    named_port_connection(NPC),
    opt_list_of_named_port_conections(NPCs).
opt_list_of_named_port_conections([]) ::= [].

named_port_connection(npc(Id,Expr)) ::=
    ['.'],
    identifier(Id),
    ['('],
    expression(Expr),
    [')'].

/**
\subsection{Behavioral Statements}
\subsubsection{Rule 61}
**/
initial_statement(initial(S)) ::= [initial], statement(S).
/**
\subsubsection{Rule 62}
**/
always_statement(always(S)) ::= [always], statement(S).

/**
\subsubsection{Rule 63}
**/
statement_or_null(null) ::= [';'].
statement_or_null(S)    ::= statement(S).

/**
\subsubsection{Rule 64}
**/
statement(if_stmt(Cond,Then,Else)) ::=
    ['if', '('],
    expression(Cond),
    [')'],
    statement_or_null(Then),
    opt_else(Else).
/**
**/
statement(case_stmt(Case,Expr,Case_items)) ::=
    case_type(Case),
    ['('],
    expression(Expr),
    [')'],
    case_items(Case_items),
    [endcase].
/**
**/
statement(forever_stmt(S)) ::=
    [forever], !,
    statement(S).
/**
**/
statement(repeat_stmt(Expr,S)) ::=
    [repeat, '('],
    expression(Expr),
    [')'],
    statement(S).
statement(while_stmt(Expr,S)) ::=
    [while, '('],
    expression(Expr),
    [')'],
    statement(S).
/**
**/
statement(for_stmt(Assgn1,Cond,Assgn2,S)) ::=
    [for, '('],
    assignment(Assgn1),
    [';'],
    expression(Cond),
    [';'],
    assignment(Assgn2),
    [')'],
    statement(S).
/**
**/
statement(d_ctrl_stmt(DC,S)) ::=
    delay_ctrl(DC), !,
    statement_or_null(S).
statement(e_ctrl_stmt(EC,S)) ::=
    event_ctrl(EC),!,
    statement_or_null(S).
/**
**/
statement(S) ::= assignment(S), [';'],!. 
/**
**/
statement(dc_assgnmt(Lval,DC,Expr)) ::=
    lvalue(Lval),
    ['='], 
    delay_ctrl(DC), !,
    expression(Expr).
statement(ec_assgnmt(Lval,EC,Expr)) ::=
    lvalue(Lval),
    ['='],
    event_ctrl(EC), !,
    expression(Expr).
/**
**/
statement(wait_stmt(Cond,S)) ::=
    [wait], !,
    ['('],
    expression(Cond),
    [')'],
    statement_or_null(S).
/**
The Thomas-Moorby BNF defines the  the non-terminal following "->" as
{\tt name\_of\_event}, but they don't define it.
We define it as an {\tt extended\_identifier}, the
definition given in the Gateway Reference Manual.
**/
statement(named_event_stmt(Id)) ::=
    ['->'], !,
    extended_identifier(Id),
    [';'].
statement(Seq_block)   ::= seq_block(Seq_block).
statement(Par_block)   ::= par_block(Par_block).
statement(Task_en)     ::= task_enable(Task_en).
statement(Sys_task_en) ::= system_task_enable(Sys_task_en).
/**
The Thomas-Moorby BNF provides two separate rules for disable:
one with {\tt name\_of\_task}
and one for {\tt name\_of\_block}. Since one cannot distinguish
between the two without semantic analysis, we combine them.
**/
statement(disable_task_block(T_Bid)) ::=
    [disable], !,
    extended_identifier(T_Bid),
    [';'].
/**
**/
statement(assign_stmt(A)) ::=  [assign],
                               assignment(A),
                               [';'].

statement(deassign_stmt(Lval)) ::= [deassign],
                                   lvalue(Lval),
                                   [';'].
/**
The Thomas-Moorby BNF does not have any rules
for the force and release statements described
in the Gateway Reference Manual.
**/
statement(force_stmt(A)) ::=
    [force], !,
    assignment(A),
    [';'].
statement(release_stmt(Lval)) ::=
    [release], !,
    lvalue(Lval),
    [';'].
statement(_,Tok_list,_) :-
    length(Tok_list,L),
    assertz(syntax_error(L,"Error in Statement")),
    !,fail.
/**
**/
opt_else(else(Stmt)) ::=
    [else],
    !, statement_or_null(Stmt).
opt_else(null) ::= [].
/**
\subsubsection{Rule 65}
**/
assignment(assgnmt(Lvalue,Expr)) ::=
    lvalue(Lvalue),
    ['='],
    expression(Expr).
/**
\subsubsection{Rule 66}
**/
case_type(case)  ::= [case].
case_type(casez) ::= [casez].
case_type(casex) ::= [casex].

case_items([Item|Items]) ::=
    case_item(Item),
    case_items(Items).
case_items([]) ::= [].

case_item(case_item(default,S)) ::=
    [default, ':'],
    statement_or_null(S).
case_item(case_item(default,S)) ::=
    [default], 
    statement_or_null(S).
case_item(case_item([E|Es],S)) ::=
    expression(E),
    opt_list_of_case_expressions(Es),
    [':'],
    statement_or_null(S).
/**
**/
opt_list_of_case_expressions([E|Es]) ::=
    [','],
    expression(E),
    opt_list_of_case_expressions(Es).
opt_list_of_case_expressions([]) ::= [].
/**
\subsubsection{Rule 67}
**/
seq_block(seq_block(Name,Decl,S)) ::=
    [begin],
    [':'],
    !,
    identifier(Name),
    opt_block_decl(Decl),
    opt_list_of_statements(S),
    [end].
seq_block(seq_block(S)) ::=
    [begin],
    opt_list_of_statements(S),
    [end].
/**
\subsubsection{Rule 68}
**/
par_block(par_block(Name,Decl,S)) ::=
    [fork],
    [':'],
    !,
    identifier(Name),
    opt_block_decl(Decl),
    opt_list_of_statements(S),
    [join].
par_block(par_block(S)) ::=
    [fork],
    opt_list_of_statements(S),
    [join].
/**
**/
opt_list_of_statements([S|Ss]) ::=
    statement(S),!,
    opt_list_of_statements(Ss). 
opt_list_of_statements([]) ::= [].

/**
\subsubsection{Rule 69}
**/
opt_block_decl(Decl) ::=  parameter_decln(Decl).
opt_block_decl(Decl) ::=  reg_decln(Decl).
opt_block_decl(Decl) ::=  integer_decln(Decl).
opt_block_decl(Decl) ::=  time_decln(Decl).
opt_block_decl(Decl) ::=  event_decln(Decl).
opt_block_decl(null) ::=  [].
/**
\subsubsection{Rule 70}
**/
task_enable(task_enable(Tid,Args)) ::=
    extended_identifier(Tid),
    opt_task_enable_expr_list(Args).

/**
\subsubsection{Rule 71}
**/
system_task_enable(sys_task_enable(Tid,Args)) ::=
    [system(Tid)],
    opt_task_enable_expr_list(Args).
/**
\subsubsection{Rule 72}
The BNF in the Gateway Reference Manual and the Thomas-Moorby book
require that there be at least one expression
in a task enable expression list
with optional additional expressions separated by 
commas. But several examples obey a different syntax. They
permit the expression between commas to be null.
This means  successive commas in such a list are legal.
The rules below conform to this syntax. Intuitively,
this makes sense, since these expressions represent
arguments to a task/procedure, and one normally
has to specify all arguments to the called procedure/task,
even if some may be empty.
**/
opt_task_enable_expr_list([]) ::=	[';'],!.
opt_task_enable_expr_list([E|Es]) ::= 
    ['('],
    expression(E), 
    task_enable_expr_list(Es),
    [';'].
task_enable_expr_list([]) ::= [')'],!.
task_enable_expr_list([E|Es]) ::= 
    [','],
    task_enable_expr(E),
    task_enable_expr_list(Es).


task_enable_expr(E) ::=	expression(E).
task_enable_expr(null) ::= [].

/**
\subsection{Specify Section}
\subsubsection{Rule 73}
**/
specify_block(specify(Items)) ::=
    [specify],
    opt_specify_items(Items),
    [endspecify].
opt_specify_items([SI|SIs]) ::=
    specify_item(SI),
    opt_specify_items(SIs).
opt_specify_items(null) ::= [].

/**
\subsubsection{Rule 74}
**/
specify_item(Sp_decl) ::= 
    specparam_declaration(Sp_decl).
specify_item(Path_decl) ::= 
    path_declaration(Path_decl).
specify_item(Lsp_decl) ::= 
    level_sensitive_path_declaration(Lsp_decl).
specify_item(Esp_decl) ::= 
    edge_sensitive_path_declaration(Esp_decl).
specify_item(Sys_tcheck) ::= 
    system_timing_check(Sys_tcheck).
/**
\subsubsection{Rule 75}
While the Thomas-Moorby BNF describes the following
as a {\tt list\_of\_assignments}, the Reference Manual BNF describes
them as a {\tt list\_of\_constant\_assignments}.
We adopted the more restrictive version.
**/
specparam_declaration(As) ::=
    [specparam],
    list_of_constant_assignments(As).
list_of_constant_assignments([A|As]) ::=
    constant_assignment(A),
    opt_list_of_constant_assignments(As).

/**
The rules below are rules for {\tt constant\_assignments} and hence
absent in the Thomas-Moorby book, since it uses assignments
instead of constant-assignments.
**/
opt_list_of_constant_assignments([]) ::= [';'],!.
opt_list_of_constant_assignments([C|Cs]) ::=
    [','],
    constant_assignment(C),
    opt_list_of_constant_assignments(Cs).

constant_assignment(const_assgnmt(C,Expr)) ::=
    identifier(C),
    ['='],
    const_mintypmax_exprn(Expr).

const_mintypmax_exprn(E) ::=
    numerical_constant(N),
    opt_more_const_mintypmax_exprn(N,E).

opt_more_const_mintypmax_exprn(N1,cmintypmax_expr(N1,N2,N3)) ::= 
    [':'], 
    numerical_constant(N2),
    [':'],
    numerical_constant(N3).
opt_more_const_mintypmax_exprn(N,N) ::= [].

/**
\subsubsection{Rule 76}
**/
path_declaration(path_decl(PDesc,PVal)) ::=
    path_description(PDesc),
    ['='],
    path_delay_value(PVal),
    [';'].
/**

\subsubsection{Rule 77}
The path description rule identifies {\tt list\_of\_path\_inputs} and
{\tt list\_of\_path\_outputs}. However, the BNF does not contain
a rule for {\tt list\_of\_path\_outputs}.
The only way to distinguish between
inputs and outputs is to checking type information,
so we leave this task for semantic analysis by
returning a pair of generic {\tt input\_output} lists.
**/
path_description(path_descr(parallel_conn,StdI,StdO)) ::=
    ['('], 
    specify_terminal_descriptor(StdI),
    ['=>'], !,
    specify_terminal_descriptor(StdO),
    [')'].
path_description(path_descr(full_conn,PIs,POs)) ::=
    ['('],
    list_of_path_inputs(PIs),
    ['*>'],
    list_of_path_outputs(POs),
    [')'].
/**
**/
list_of_path_inputs(Ps) ::=   list_of_path_inputs_outputs(Ps).
list_of_path_outputs(Ps) ::=   list_of_path_inputs_outputs(Ps).

list_of_path_inputs_outputs([Std|Stds]) ::=
    specify_terminal_descriptor(Std),
    opt_list_of_path_inputs_outputs(Stds).

opt_list_of_path_inputs_outputs([Std|Stds]) ::=
    [','], 
    specify_terminal_descriptor(Std),
    opt_list_of_path_inputs_outputs(Stds).
opt_list_of_path_inputs_outputs([]) ::= [].

/**
\subsubsection{Rule 78}
Thomas-Moorby describes {\tt specify\_terminal\_descriptor}
rule with 2 rules: {\tt specify\_input\_terminal\_descriptor} and
{\tt specify\_output\_terminal\_descriptor}.
Since this check requires semantic
analysis, we currently  use just one rule.
**/
specify_terminal_descriptor(spec_term_desc(Id,Expr)) ::=
    extended_identifier(Id),
    opt_bracket_expression(Expr).

opt_bracket_expression(bit_select(E1)) ::=
    ['['],
    constant_expression(E1),
    [']'].
opt_bracket_expression(part_select(E1,E2)) ::=
    ['['],
    constant_expression(E1),
    [':'],
    constant_expression(E2),
    [']'].
opt_bracket_expression(null) ::= [].
/**

\subsubsection{Rule 79}
**/
path_delay_value(path_del_val(Pde)) ::=
    path_delay_expression(Pde).

path_delay_value(path_del_val(PdE1,PdE2)) ::=
    ['('],
    path_delay_expression(PdE1),
    [','],
    path_delay_expression(PdE2),
    [')'].

path_delay_value(path_del_val(PdE1,PdE2,PdE3)) ::=
    ['('],
    path_delay_expression(PdE1),
    [','],
    path_delay_expression(PdE2),
    [','],
    path_delay_expression(PdE3),
    [')'].

path_delay_value(path_del_val(PdE1,PdE2,PdE3,PdE4,PdE5,PdE6)) ::=
    ['('],
    path_delay_expression(PdE1),
    [','],
    path_delay_expression(PdE2),
    [','],
    path_delay_expression(PdE3),
    [','],
    path_delay_expression(PdE4),
    [','],
    path_delay_expression(PdE5),
    [','],
    path_delay_expression(PdE6),
    [')'].
/**
\subsubsection{Rule 80}
**/
path_delay_expression(path_del_exprn(Expr))  ::=
    expression(Expr).
/**
\subsubsection{Rule 81}
**/
system_timing_check(sys_tim_check('$setup',TE1,TE2,Tcl,NR)) ::=
    [system('$setup')], !,
    ['('],
    timing_check_event(TE1),
    [','],
    timing_check_event(TE2),
    [','],
    timing_check_limit(Tcl),
    opt_notify_register(NR),
    [')'],
    [';'].
/**
**/
system_timing_check(sys_tim_check('$hold',TE1,TE2,Tcl,NR)) ::=
    [system('$hold')], !,
    ['('],
    timing_check_event(TE1),
    [','],
    timing_check_event(TE2),
    [','],
    timing_check_limit(Tcl),
    opt_notify_register(NR),
    [')'],
    [';'].
/**
**/
system_timing_check(sys_tim_check('$period',CTcE,TL,NR)) ::=
    [system('$period')], !,
    ['('],
    controlled_timing_check_event(CTcE),
    [','],
    timing_check_limit(TL),
    opt_notify_register(NR),
    [')'],
    [';'].
/**
**/
system_timing_check(sys_tim_check('$width',CTcE,TL,CE_NR)) ::=
    [system('$width')],!,
    ['('],
    controlled_timing_check_event(CTcE),
    [','],
    timing_check_limit(TL),
    opt_const_exprn_notify_register(CE_NR),
    [')'],
    [';'].
/**
**/
system_timing_check(sys_tim_check('$skew',TE1,TE2,Tcl,NR)) ::=
    [system('$skew')],!,
    ['('],
    timing_check_event(TE1),
    [','],
    timing_check_event(TE2),
    [','],
    timing_check_limit(Tcl),
    opt_notify_register(NR),
    [')'],
    [';'].
/**
**/
system_timing_check(sys_tim_check('$recovery',CTcE,TE,TL,NR)) ::=
    [system('$recovery')], !,
    ['('],
    controlled_timing_check_event(CTcE),
    [','],
    timing_check_event(TE),
    [','],
    timing_check_limit(TL),
    opt_notify_register(NR),
    [')'],
    [';'].
/**
**/
system_timing_check(sys_tim_check('$setuphold',TE1,TE2,TL1,TL2,NR)) ::=
    [system('$setuphold')], !,
    ['('],
    timing_check_event(TE1),
    [','],
    timing_check_event(TE2),
    [','],
    timing_check_limit(TL1),
    [','],
    timing_check_limit(TL2),
    opt_notify_register(NR),
    [')'],
    [';'].
/**
\subsubsection{Rule 82}
**/
timing_check_event(tchk_event(Ec,Std,Cond)) ::=
    opt_timing_check_event_control(Ec),
    specify_terminal_descriptor(Std),
    opt_timing_check_condition(Cond).
/**

\subsubsection{Rule 83}
**/
controlled_timing_check_event(controlled_tchk_event(Ec,Std,Cond)) ::=
    timing_check_event_control(Ec),
    specify_terminal_descriptor(Std),
    opt_timing_check_condition(Cond).
/**

\subsubsection{Rule 84}
The Thomas-Moorby BNF does not list an {\tt  edge\_control}  and
{\tt edge\_descriptor} rule that is listed in the Gateway Reference
Manual. They are included here.
**/
timing_check_event_control(tc_event_ctrl(posedge)) ::= 
    [posedge],!.
timing_check_event_control(tc_event_ctrl(negedge)) ::= 
    [negedge],!.
timing_check_event_control(tc_event_ctrl(Edge_ctrl)) ::= 
    edge_control(Edge_ctrl).

opt_timing_check_event_control(Ec) ::=
    timing_check_event_control(Ec),!.
opt_timing_check_event_control(null) ::= [].
/**
**/
edge_control([Ed|Eds]) ::=
    [edge],
    ['['],[Mark],
    edge_descriptor(Mark,Ed),
    opt_edge_descriptor_list(Eds).
/**

**/
opt_edge_descriptor_list([]) ::= [']'].
opt_edge_descriptor_list([Ed|Eds]) ::=
    [','],[Mark],
    edge_descriptor(Mark,Ed),
    opt_edge_descriptor_list(Eds).
/**

**/
edge_descriptor(number([0'0,0'1]),01) ::= !, [].
edge_descriptor(number([0'1,0'0]),10) ::= !, [].
edge_descriptor(number([0'0]),'0x') ::=	 [x],!.
edge_descriptor(number([0'1]),'1x') ::=	[x],!.
edge_descriptor('x','x0') ::=	[number([0'0])],!.
edge_descriptor('x','x1') ::=	[number([0'1])],!.	
/**

\subsubsection{Rule 85}
**/
opt_timing_check_condition(Tc_cond) ::=
    ['&&&'],!,
    timing_check_condition(Tc_cond).
opt_timing_check_condition(null) ::= [].

timing_check_condition(tcheck_cond(Sexp)) ::=
    ['('],
    timing_check_condition(Sexp),
    [')'].
timing_check_condition(tcheck_cond(Sexp)) ::=
    scalar_expression(Sexp).
timing_check_condition(tcheck_cond('~',Sexp)) ::=
    ['~'],
    scalar_expression(Sexp).
timing_check_condition(tcheck_cond(Sexp,Scalar_op,Scalar_const)) ::=
    scalar_expression(Sexp),
    scalar_expr_op(Scalar_op),
    scalar_constant(Scalar_const).
/**
\subsubsection{Rule 86}
A {\tt scalar\_expression} is defined in the Thomas-Moorby 
book as ``a one bit net or a bit select of an expanded
vector net''.  Since a one bit net is an identifier,
and a bit select of an expanded vector net is 
{\tt bit\_select} expression, we parse an identifier and then
look for an optional {\tt bit\_select} expression.
**/
scalar_expression(Sexp) ::= 
    identifier(Id),	
    opt_bitselect(Id,Sexp).
/**
**/
opt_bitselect(Id,bit_select(Id,Expr)) ::=
    ['['],
    !,
    expression(Expr),
    [']'].
opt_bitselect(Id,Id) ::= [].

scalar_expr_op('==') ::= ['=='].
scalar_expr_op('===') ::= ['==='].
scalar_expr_op('!=') ::= ['!='].
scalar_expr_op('!==') ::= ['!=='].

/**
\subsubsection{Rule 87}
**/
timing_check_limit(tim_check_lim(Ce)) ::=
    constant_expression(Ce).
/**

\subsubsection{Rule 88}
More terminals are added to the grammar rules for
a {\tt scalar\_constant} (a combination of those in two BNFs).
**/
scalar_constant(0) ::=  [base_num(b,[0'0])],!.
scalar_constant(1) ::= [base_num(b,[0'1])], !.
scalar_constant(0) ::=  [number([0'0])],!.
scalar_constant(1) ::=  	[number([0'1])], !, [base_num(b,[0'1])].
scalar_constant(0) ::=  	[number([0'1])], !,[base_num(b,[0'0])].
scalar_constant(1) ::= 	[number([0'1])].
/**

\subsubsection{Rule 89}
**/

opt_notify_register(notify_reg(NR)) ::=
    [','],!,
    extended_identifier(NR).	
opt_notify_register(null) ::= [].
/**
**/
opt_const_exprn_notify_register(cexpr_notify_reg(Ce,Nr)) ::=
    [','],
    constant_expression(Ce), 
    [','],
    extended_identifier(Nr).
opt_const_exprn_notify_register(null) ::= [].

/**

\subsubsection{Rule 90}
**/
level_sensitive_path_declaration(
     lsp_decl(Cond,parallel_conn(Std1,Op,Std2,Pdv))) ::=
    [if],
    ['('],
    conditional_port_expression(Cond),
    [')'],
    ['('],
    specify_terminal_descriptor(Std1),
    opt_polarity_operator(Op),
    ['=>'],
    !,
    specify_terminal_descriptor(Std2),
    [')'],
    ['='],
    path_delay_value(Pdv).
level_sensitive_path_declaration(
     esp_decl(Cond,full_conn(PIs,Op,POs,Pdv))) ::=
    [if],
    ['('],
    conditional_port_expression(Cond),
    [')'],
    ['('],
    list_of_path_inputs(PIs),
    opt_polarity_operator(Op),
    ['*>'],
    list_of_path_outputs(POs),
    [')'],
    ['='],
    path_delay_value(Pdv).
/**

\subsubsection{Rule 91}
**/
conditional_port_expression(cond_port_expr(Op,Ref)) ::=
    [Tok],
    unary_operator(Tok,Op),
    !,
    port_reference(Ref).
conditional_port_expression(cond_port_expr(Ref)) ::=
    port_reference(Ref).
conditional_port_expression(cond_port_expr(Ref1,Bin_op,Ref2)) ::=
    port_reference(Ref1),
    [Tok],
    binary_operator(Tok,Bin_op,_),
    port_reference(Ref2).
/**

\subsubsection{Rule 92}
**/
polarity_operator('-') ::=	['-'],!.
polarity_operator('+') ::=	['+'].


opt_polarity_operator(P) ::= polarity_operator(P).
opt_polarity_operator(null) ::= [].
/**
\subsubsection{Rule 93}
**/
edge_sensitive_path_declaration(esp_decl(Cond,Edge_id,
                              body('=>',Std1,Std2,Op,Expr,Pdv))) ::=
    opt_esp_cond_exp(Cond),
    ['('],
    opt_edge_id(Edge_id),
    specify_terminal_descriptor(Std1),
    ['=>'], !,
    ['('],
    specify_terminal_descriptor(Std2),
    polarity_operator(Op),
    ['?:'],
    expression(Expr),
    [')'],
    [')'],
    ['='],
    path_delay_value(Pdv).
edge_sensitive_path_declaration(esp_decl(Cond,Edge_id,
                              body('*>',Std1,POs,Op,Expr,Pdv))) ::=
    opt_esp_cond_exp(Cond),
    ['('],
    opt_edge_id(Edge_id),
    specify_terminal_descriptor(Std1),
    ['*>'],
    ['('],
    list_of_path_outputs(POs),
    polarity_operator(Op),
    ['?:'],
    expression(Expr),
    [')'],
    [')'],
    ['='],
    path_delay_value(Pdv).
/**
**/
opt_esp_cond_exp(Cond) ::=
    [if],!,
    ['('],
    expression(Cond),
    [')'].
opt_esp_cond_exp(null) ::= [].


/**

\subsubsection{Rule 94}
**/
opt_edge_id(posedge) ::= [posedge].
opt_edge_id(negedge) ::= [negedge].
opt_edge_id(null) ::= [].
/**

\subsection{Expressions}
\subsubsection{Rule 95}
**/
lvalue(Id) ::= 	extended_identifier(Id).
lvalue(select(Id,Expr)) ::= 
    extended_identifier(Id),
    opt_bracket_expression(Expr).
lvalue(L) ::= concatenation(L).

/**

\subsubsection{Rule 96}
**/
constant_expression(Ce) ::= expression(Ce).
/**

\subsubsection{Rule 97}
**/
mintypmax_expr(Me) ::= 
    expression(E1),
    opt_rest_mintypmax_expr(E1,Me).

opt_rest_mintypmax_expr(E1,mintypmax_expr(E1,E2,E3)) ::=
    [':'],
    !,
    expression(E2),
    [':'],
    expression(E3).
opt_rest_mintypmax_expr(E,E) ::= [].
/**

\subsubsection{Rule 98}
The expression parsing mechanism in MULTI/PLEX
requires only the definitions of a few primary-expressions
and operator definitions.
An important note here is that the Thomas-Moorby book,
indicates  that  an {\tt expression} can be a {\tt mintypmax\_expression}
(via the rule for a {\tt primary}). But according to the BNF the 
{\tt mintypmax\_expression} has to be enclosed in parentheses.
But this does not appear to be the case since several examples
did not obey this rule. Hence we had to add a rule to permit
this, while taking care a circuilar loop of productions
is not generated, since a {\tt mintypmax\_expression} also
has a rule that makes it an {\tt expression}.

\subsubsection{Rule 99}
**/
operator(100, fx,  plus) ::= ['+'].
operator(100, fx, minus) ::= ['-'].
operator(100, fx, not)   ::= ['!'].
operator(100, fx, not)   ::= ['~'].
operator(100, fx, and)   ::= ['&'].
operator(100, fx, nand)  ::= ['~&'].
operator(100, fx, or)    ::= ['|'].
operator(100, fx, nor)   ::= ['~|'].
operator(100, fx, xor)   ::= ['^'].
operator(100, fx, xnor)  ::= ['~^'].
operator(100, fx, xnor)  ::= ['^~'].
/**
\subsubsection{Rule 100}
The ternary operator ? has lowest precedence. Not mentioned here, It's
expression is constructed at the topmost level ensuring that it
follows all other subexpressions.
**/
operator(200, xfy, '||') ::= ['||'].
operator(300, xfy, '&&') ::= ['&&'].
operator(400, xfy, '|' ) ::= ['|'].
operator(500, xfy, '^' ) ::= ['^'].
operator(500, xfy, '^~') ::= ['^~'].
/**
**/
operator(500, xfy, '~^') ::= ['~^'].
operator(600, xfy, '&' ) ::= ['&'].
operator(700, xfy, '==') ::= ['=='].
operator(700, xfy, '!=') ::= ['!='].
operator(700, xfy, '===')::= ['==='].
operator(700, xfy, '!==')::= ['!=='].
/**
**/
operator(800, xfy, '<')  ::= ['<'].
operator(800, xfy, '<=') ::= ['<='].
operator(800, xfy, '>')  ::= ['>'].
operator(800, xfy, '>=') ::= ['>='].
/**
**/
operator(900, xfy, '<<')  ::= ['<<'].
operator(900, xfy, '>>')  ::= ['>>'].
/**
**/
operator(1000, xfy, plus) ::= ['+'].
operator(1000, xfy, minus) ::= ['-'].
/**
**/
operator(1100, xfy, times)  ::= ['*'].
operator(1100, xfy, divide) ::= ['/'].
operator(1100, xfy, mod)    ::= ['%'].
/**
\subsubsection{Rule 101}
**/
primary(P) ::= 	['('], expression(P), [')'].
primary(P) ::= numerical_constant(P).
primary(P) ::= extended_identifier(P).
primary(bit_select(P,E)) ::= 
    extended_identifier(P),
    ['['],
    expression(E),
    [']'],!.
primary(part_select(P,E1,E2)) ::= 
    extended_identifier(P),
    ['['],
    constant_expression(E1),
    [':'],!,
    constant_expression(E2),
    [']'].
primary(P) ::= concatenation(P).
primary(P) ::= multi_concatenation(P).
primary(P) ::= func_call(P).
primary(P) ::= 	['('], !, mintypmax_expr(P), [')'].
/**
\subsubsection{Rule 102}
A numerical constant can be either a normal decimal number, or
a string of characters of the form:

\centerline{\tt ss...s 'fnnn..n }

where the characters {\tt sss...s} are optional.
They represent a size specification of a base string.
The {\tt 'fnnnnnn} is recognized by the tokenzier as a
token of the form {\tt base\_num(Base,Str)}. 
**/
numerical_constant(base_num(Base_num)) ::=  
    [base_string(Base,Str)],
    {atom_chars(Base,[Ch|_]),
    name(Base_num,[0'',Ch|Str])},!.
numerical_constant(NC) ::=
    [number(Ns)],
    {name(Num,Ns)},
    opt_base_string(Num,NC).

opt_base_string(Size,base_num(Size,Base_num)) ::=
    [base_string(Base,Str)],
    {atom_chars(Base,[Ch|_]),
    name(Base_num,[0'',Ch|Str])},
    !.
opt_base_string(Num,Num) ::= [].
/**


\subsubsection{Rule 103}
Concatenation does not permit unsized constant numbers
in its expression. This includes  number(Num) and base\_num(Base,Str)
On the other hand base\_num(Size,Base,Str) is allowed.
We do not check for this here.
**/
concatenation(concat([C|Cs])) ::=
    ['{'],
    expression(C), 
    opt_concat_explist(Cs).
/**

**/
opt_concat_explist([]) ::= ['}'],!.
opt_concat_explist([E|Es]) ::=
    [','],
    expression(E), 
    opt_concat_explist(Es).
/**

\subsubsection{Rule 104}
**/
multi_concatenation(concat(Rep_amt,concat([C|Cs]))) ::=
    ['{'],
    expression(Rep_amt), 
    ['{'],
    expression(C), 
    opt_concat_explist(Cs),
    ['}'].
/**


\subsubsection{Rule 105}
**/
func_call(func_call(system(Id),Es)) ::=
    [system(Id)], !,
    opt_func_call_args(Es).
func_call(func_call(Id,[E|Es])) ::=
    identifier(Id),
    ['('],
    expression(E),
    opt_list_of_expressions(Es).

opt_func_call_args([E|Es])  ::=
    ['('],!,
    expression(E),
    opt_list_of_expressions(Es).
opt_func_call_args(null) ::= [].

/**
\subsection{General}
\subsubsection{Rule 106}
An identifier must not be a keyword-token.
An extended identifier is one or more identifiers
connected with the period character. The individual identifiers 
are collected and then concatenated together with the
'.' character to yield the extended identifier.
**/
extended_identifier(Eid) ::= 
    identifier(Id),
    opt_extended_identifier(Ids),
    {create_extended_id([Id|Ids],Eid)}.
/**
**/
opt_extended_identifier([Id|Ids]) ::=
    ['.'],
    identifier(Id),
    opt_extended_identifier(Ids).
opt_extended_identifier([])  ::= [].
/**
\subsubsection{Rule 107}
**/
identifier(Id) ::=
    [Id],
    {verilog_token(Id) -> fail; true}.

opt_identifier(Id) ::= identifier(Id).
opt_identifier(null) ::= [].
/**
**/
create_extended_id([Id],Id) :- !.
create_extended_id([Id1,Id2],Ext_id) :- 
    concat_atom([Id1,'.',Id2],Ext_id),!.
create_extended_id([Id1,Id2|Ids], Ext_id) :-         
    concat_atom([Id1,'.',Id2],Prefix),
    create_extended_id(Ids,Suffix),
    concat_atom([Prefix, '.',Suffix],Ext_id).

/**

\subsubsection{Rule 108}
The Thomas-Moorby BNF permits a maximum of only two (minimum of 0)
additional {\tt mintypmax\_expr} in the first rule for delay//1, while the 
Gateway Reference manual permits any number. We chose the restrictive
version.
**/

opt_delay(D) ::= delay(D).
opt_delay(null) ::= [].

delay(mintypmax_expr(E,E1,E2)) ::=
    ['#'],
    ['('], !,
    mintypmax_expr(E),
    opt_mintypmax_expr(E1),
    opt_mintypmax_expr(E2),
    [')'].
delay(D) ::=
    ['#'],
    numerical_constant(D).
delay(D) ::=
    ['#'],
    extended_identifier(D).

opt_mintypmax_expr(E) ::= [','], !, mintypmax_expr(E).
opt_mintypmax_expr(null) ::= [].

/**

\subsubsection{Rule 109}
The Thomas-Moorby rule for the first clause of {\tt delay\_ctrl//1}
has a {\tt mintypmax\_expr//1}
and not an expression//1 (as given in the
Gateway Reference Manual)
**/
opt_delay_ctrl(D) ::= delay_ctrl(D).
opt_delay_ctrl(null) ::= [].
delay_ctrl(d_ctrl(Expr)) ::=
    ['#'],
    ['('],
    !,
    mintypmax_expr(Expr),
    [')'].
delay_ctrl(d_ctrl(N)) ::= 
    ['#'],
    numerical_constant(N).
delay_ctrl(d_ctrl(Name)) ::=
    ['#'],
    extended_identifier(Name).


/**
\subsubsection{Rule 110}
**/
event_ctrl(e_ctrl(Expr)) ::=
    ['@'],
    ['('],
    !,
    event_expression(Expr), 
    [')'].
event_ctrl(e_ctrl(Name)) ::= 
    ['@'],
    extended_identifier(Name).

/**
\subsubsection{Rule 111}
**/
event_expression(Result) ::=
    [posedge], !,
    expression(Expr),
    opt_binary_event_expression(ev_expr(posedge,Expr),Result).
event_expression(Result) ::=
    [negedge], !,
    expression(Expr),
    opt_binary_event_expression(ev_expr(negedge,Expr),Result).
event_expression(Result) ::=
    expression(Expr),
    opt_binary_event_expression(ev_expr(Expr),Result).
/**
**/
opt_binary_event_expression(Expr1,ev_expr(Expr1,or,Expr2)) ::=
    [or],
    !,
    event_expression(Expr2).
opt_binary_event_expression(Expr,Expr) ::= [].
