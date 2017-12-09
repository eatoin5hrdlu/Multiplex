%:- use_module(library(multi)).
:- use_module(multi).
:- use_module(library(strings),  [concat_atom/2]).

runtime_entry(start) :-
    unix(argv(Args)),
    multiplex_set_options(Args, [File]),

    name(File, FChs),
    ( append(LChs,".spec", FChs) ->
          name(Language, LChs)
    ; File = Language
    ),

    assert(multi:language(Language)),
    multiplex_build(Language),
    multiplex_quit.

