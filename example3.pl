:- load_files(library(plex), [when(both)]).

xyz lexicon

"start"                           is [start|R] if switch(qua,R);

"[0-9]+"                          is number(N)
                                  if (text(T), number_chars(N,T)) ;
"[0-9]+\.[0-9]+([eE]-?[00-9]+)?"  is number(N)
                                  if (text(T), float_chars(N,T));
"[A-Za-z][A-Za-z_0-9-]*"          is  id(ID)
                                  if (text(T), name(ID,T)) ;
"""[^""]*"""                      is  quote(T)
                                  if  text(T) ;
"[ \t\n]"                         is  T if switch(xyz,T);
"."                               is  C
                                  if (text(T), name(C,T)).


qua lexicon

"atom"       is atomic;
"term"       is termlish;
"[ \t\n]*"   is T if switch(qua,T);
"end"        is [end|T] if switch(xyz,T).

runtime_entry(start) :-
     repeat,
       (get_line(L) ->
               xyz_parse:xyz(L,T),
               write(T),nl,
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



