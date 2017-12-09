
german lexicon

"[A-Za-z]+"  is Word  if (text(T),name(Word,T)) ;
"[ ,\t\n]"   is R     if switch(german, R).

german ::=
   file('name.german',german),
   subjeckt,
   opt_subjeckts.

opt_subjeckts ::= subjeckt, opt_subjeckts.
opt_subjeckts ::= [].

subjeckt ::= artikel(Gender), hauptwort(N,Gender),
             update(subject(N)).

hauptwort(dog, mannlich) ::= [ 'Hund' ].
hauptwort(cat, weiblich) ::= [ 'Katze' ].

artikel(mannlich) ::= [ der ].
artikel(weiblich) ::= [ die ].
artikel(sachlich) ::= [ das ].


