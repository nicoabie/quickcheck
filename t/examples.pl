:- use_module(library(quickcheck)).

% reversing a list leaves its length the same
prop_reverse_length(L:list(integer)) :-
    length(L, Len),
    reverse(L, RL),
    length(RL, Len).


% a property that's never true
prop_nonsense(A:list(integer), B:list(integer)) :-
    length(A, LenA),
    length(B, LenB),
    LenA =< LenB.

:- use_module(library(tap)).

'reverse does not change length' :-
    quickcheck(prop_reverse_length/1).

'reverse changes length'(fail) :-
    quickcheck(prop_nonsense/2).
