:- use_module(library(quickcheck)).

prop_reverse_length(L:list(atomic)) :-
    length(L, Len),
    reverse(L, RL),
    length(RL, Len).

:- use_module(library(tap)).

'reverse does not change length' :-
    quickcheck(prop_reverse_length/1).
