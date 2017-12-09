:- use_module(multi).

runtime_entry(start) :-
     unix(argv(Args)),
     multiplex_set_options(Args, [In, Out]),
     multiplex_input(  In, Data),
     format(user_error,"Finished input phase~n",[]),
     multiplex_output(Out, Data).

runtime_entry(start) :-
    format('Usage: translate ~a ~a ~a~n',
          [ '[-v(erbose)]', 'input.lang1', 'output.lang2' ]).


