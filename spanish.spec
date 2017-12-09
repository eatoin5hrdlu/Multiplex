
spanish lexicon
"[a-z]+"    is Word  if (text(T), name(Word,T));
"[ ,\t\n]"  is R     if switch(spanish,R).

spanish ::=
    file('name.spanish',spanish),
    subjectivo,
    opt_subjectivos.

subjectivo ::= articulo(Gender), nombre(N,Gender),
               update(subject(N)).

opt_subjectivos ::= subjectivo, opt_subjectivos.
opt_subjectivos ::= [].

nombre(dog, masculino) ::= [ perro ].
nombre(cat, masculino) ::= [ gato ].

articulo(masculino) ::= [ el ].
articulo(feminina)  ::= [ la ].

