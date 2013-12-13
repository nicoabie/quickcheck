:- use_module(library(quickcheck)).

% reversing a list leaves its length the same
prop_reverse_length(L:list(atomic)) :-
    length(L, Len),
    reverse(L, RL),
    length(RL, Len).


% a property that's never true
prop_reverse_length_wrong(_:list(atomic)) :-
    fail.

:- use_module(library(tap)).

'reverse does not change length' :-
    quickcheck(prop_reverse_length/1).

'reverse changes length'(fail) :-
    quickcheck(prop_reverse_length_wrong/1).
