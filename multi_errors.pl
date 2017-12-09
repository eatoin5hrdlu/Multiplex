
:- multifile user:message_hook/3.

user:(message_hook(M,_,_) :- multi:my_message_hook(M)).

my_message_hook(M) :-
     functor(M, Type, _),
     ( ignore(Type) -> true 
     ; proper_indeterminate(Type, Ind),
       format('MULTIPLEX detected ~a ~a~n', [Ind, Type]),
       ( language(Lang)   -> true
       ; arg(2,M,Mod),atom(Mod) ->
                     name(Mod,Chs),
                     append(Lingo,[0'_,0'p|_],Chs),
                     name(Lang,Lingo)
       ; language_loaded(Lang) -> true
       ; Lang = '<<unknown_language>>'
       ),
       report(Type, M, Lang),
       ( warning(Type) -> true ; abort )
     ).

ignore(top_level).
ignore(top_level_goal).
ignore(term_reading).
ignore(load_completed).
ignore(loading_file).
ignore(loading).
ignore(consulting).
ignore(saving_file).
ignore(saved_file).
ignore(saving).

warning(syntax_error).
warning(singleton_variables).

report(existence_error, M, Lang) :-
     arg(1, M, Goal),
     functor(Goal, Functor, RealArity),
     (RealArity > 3 -> Arity is RealArity - 4
     ; Arity = RealArity
     ),
     plural(Arity,Plural),
     plural(RealArity,RAPlural),
     format('~+You may be missing a definition or have~n',[]),
     format('~+an unnecessary call for grammar rule:~n~n',[]),
     format('~+~+"~a", with ~d argument~a~n', [Functor, Arity, Plural]),
     format('~+~+"~a", (internal ~d argument~a)~n~n', [Functor, RealArity, RAPlural]),
     format('~+in specification file "~a.spec"~n~n', [Lang]).

report(singleton_variables, M, Lang) :- bad_variables(Lang,M).
report(instantiation_error, M, Lang) :- bad_variables(Lang,M).
report(type_error,          M, Lang) :- bad_variables(Lang,M).

report('syntax error', M, Lang) :-
     current_name(Name),
     format('~+In file "~a.~a"~n~n',[Name,Lang]),
     arg(3,M,Left),
     arg(4,M,Right),
     arg(5,M,Message),
     write('    '), write_list(Left),
                    write( '<<<HERE>>> '),
                    write_list(Right),nl,nl,
     (Message == null -> true
     ; write('       MESSAGE: '),write(Message),nl
     ).

report(syntax_error, M, Lang) :-
     arg(2,M,between('$stream_position'(_,Line,_,_,_),_)),
     format('~+On line ~d of specification file "~a.spec"~n~n',[Line,Lang]),
     arg(4,M,Left),
     arg(5,M,Right),
     format('~+~s  <HERE> ~s~n~n',[Left,Right]).

report('lexicon error', M, Lang) :-
     arg(1,M,Rule),
     format('~+in specification file "~a.spec"~n~n', [Lang]),
     format('~+Rule was: ~p~n~n',[Rule]).

report('lexical error', M, Lang) :-
     arg(1,M,lexicon(Lex,F/A)),
     !,
     format('~+Internal string cannot be read with the lexical~n', []),
     format('~+rules "~a" in file "~a.spec".~n', [Lex,Lang]),
     format('~+This error occurred in parse_string//3 with goal ~a/~d~n',[F,A]),
     format('~+MULTIPLEX cannot determine which is at fault.~n~n', []),
     fail.

report('lexical error', M, Lang) :-
     arg(1,M,DataFile),
     format('~+Data file "~a" cannot be read with~n', [DataFile]),
     format('~+the lexical rules in file "~a.spec".~n', [Lang]),
     format('~+MULTIPLEX cannot determine which is at fault.~n~n', []).

report('expression error', M, _) :-
     arg(1,M,List),
     length(L,10),
     append(L,_,List),
     format('~+Expression expected in call to built-in~n', []),
     format('~+expression parser. Input was:~n~n', []),
     format('~+~s~n~n',[L]).

report('parsing error',M, Lang) :-
     arg(1,M,Functor/Arity),
     !,
     format('~+Internal string cannot be parsed with~n', []),
     format('~+grammar rule ~a/~d in file "~a.spec".~n', [Functor,Arity,Lang]),
     format('~+MULTIPLEX cannot determine which is at fault.~n', []),
     format('~+Try adding grammar rules with error productions.~n~n', []).

report('parsing error',M, Lang) :-
     arg(1,M,DataFile),
     format('~+Data file "~a" cannot be parsed with~n', [DataFile]),
     format('~+the grammar rules in file "~a.spec".~n', [Lang]),
     format('~+MULTIPLEX cannot determine which is at fault.~n', []),
     format('~+Try adding grammar rules with error productions.~n~n', []).

report('printing error',M, Lang) :-
     arg(1,M,DataFile),
     format('~+Data file "~a" cannot be generated from the~n', [DataFile]),
     format('~+database and the grammar rules in file "~a.spec".~n', [Lang]),
     format('~+MULTIPLEX cannot determine which is at fault.~n~n', []).

report('string printing error',M, Lang) :-
     arg(1,M,F/A),
     format('~+String cannot be generated from grammar~n',[]),
     format('~+rule ~a/~d in file "~a.spec".~n~n', [F,A,Lang]).

bad_variables(Lang,M) :-
     format('~+This error can be caused by the incorrect use of~n',[]),
     format('~+variables (upper-case names) in the grammar rules~n',[]),
     format('~+in specification file "~a.spec"~n~n', [Lang]),
     write(M),nl.

proper_indeterminate(Name, Ind) :-
     name(Name,Chars),
     append([V|_],[T],Chars),
     ( plural(T) -> Ind = ''
     ; vowel(V)  -> Ind = an
     ;              Ind = a
     ).

plural(0's).
plural(0'S).

vowel(0'a).  vowel(0'A).
vowel(0'e).  vowel(0'E).
vowel(0'i).  vowel(0'I).
vowel(0'o).  vowel(0'O).
vowel(0'u).  vowel(0'U).

plural(1,'') :- !.
plural(_,s).

write_list([]).
write_list([';'|Ts]) :-
     !,
     write(';'),nl,write('     '),
     write_list(Ts).

write_list([quote(double(L))|Ts]) :-
     !,
     format(' "~s" ', [L]),
     write_list(Ts).

write_list([T|Ts]) :-
     write(' '), write(T),
     write_list(Ts).

