
sample lexicon

"start"                           is [start|R] if switch(qua,R);

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

qua lexicon

"atom"       is atomic;
"term"       is termlish;
"[ \t\n]*"   is T if switch(qua,T);
"end"        is [end|T] if switch(sample, T).
