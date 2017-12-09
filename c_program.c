/**
\documentstyle[11pt]{article}
\hfuzz 8pt
\begin{document}
\title{Using MULTI/PLEX with C Programs}
\author{Peter Reintjes \\
DAZIX/Intergraph}
\maketitle

Before using MULTI/PLEX as a language interface
for a C program, I would like to recommend 
"Logic Programming and Compiler Writing" by
David H. D. Warren in Vol 10, Number 2 (1980) 
issue of {\it Software -- Practice and Experience}.
This article explains the many advantages of using
Prolog to develop language-handling tools.

Of course, if you must use C, you can still use MULTI/PLEX.
The following commands produce an executable by linking a
C program with the MULTI/PLEX module.
\begin{verbatim}
      % cc -c c_program.c
      % qpc -c multi.pl
      % qld -Dd multi.qof c_program.o -o c_program
\end{verbatim}
A C program which calls MULTI/PLEX is begun by
including the Quintus definitions file and defining a few
macros to simplify data-structure access.
**/
#include <quintus/quintus.h>
#define ID(T)         arg_string(1,T)
#define Type(T)       arg_string(2,T)
#define Value(T)      arg_string(3,T)
#define First(T,F,A)  QP_get_functor(T,&F,&A)
#define Next(T,F,A)  (QP_get_arg(4,T,T),QP_get_functor(T,&F,&A))
/**
The {\tt arg\_string} macro converts atomic
arguments of {\tt Tree} to character strings and will
return the string "not-atom" for a non-atomic argument.
**/
#define arg_string(N, T) \
    (QP_get_arg(N,T,Arg), \
    (QP_get_atom(Arg,&A)?QP_string_from_atom(A):"not-atom"))
/**
The program must first initialize the Prolog system,
create a ``pred-reference'' initialized to the predicate
we wish to call, and a ``term-reference'' for the Prolog
structure which will be returned by the parser.
**/
main(argc, argv)
int    argc;
char **argv;
{
int         status = QP_initialize(argc, argv);
QP_pred_ref Input  = QP_predicate("multiplex_input",2,"multi");
QP_term_ref Tree   = QP_new_term_ref();
QP_term_ref Arg    = QP_new_term_ref(); /* used by arg_string */
unsigned long Functor, A /* used by arg_string */;
int           Arity;
/**
After declaring variables to hold a functor, argument,
and arity of the term we will be ``dismantling'',
we call the multiplex input predicate with the file name
given on the command line.
**/
  status = QP_query(Input, argv[1], Tree);
/**
Finally, we traverse the {\tt x(ID,Type,Value,x(I2,T2,V2,...)}
data structure in the following loop.
**/
  for( First(Tree, Functor, Arity) ;
       Arity == 4 ;
       Next(Tree, Functor, Arity) )
  {
     printf("Item(%s,%s,%s)\n",ID(Tree),Type(Tree),Value(Tree));
  }
} /* End of Program */
/**
\end{document}
**/
