:- use_module(plex).
:- use_module(library(strings), [concat_atom/2]).


runtime_entry(start) :-
     unix(argv([File])),                      % e.g. 'data.sample'
     name(File, FileChs),
     append(_, [0'.|LangChs], FileChs),
     name(Lang, LangChs),
     concat_atom([Lang,'.spec'], SpecFile),   % e.g. 'data.spec'
     concat_atom([Lang,'_parse'], Module),    % e.g  'data_parse'

     consult(SpecFile),           % BUILD LEXICAL ANALYZER

     Lexer =.. [Lang, Chs, Tks], 
     see(File),
     repeat,
       (get_line(Chs) ->
               call(Module:Lexer), % CALL LEXICAL ANALYZER
               write(Tks),nl,
               fail
       ; true
       ),
     seen.


get_line(Line) :-
   get0(C),
   get_line(C,Line).

get_line(-1,   _)  :- !, fail.
get_line(10,[10])  :- !.
get_line(C,[C|Cs]) :-
   get0(NC),
   get_line(NC,Cs).

