:- use_module(library(quickcheck)).
:- use_module(library(lists)).

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

% atom_length/2 and length/2 agree
prop_atom_lengths(A:atom) :-
    atom_length(A, Len),
    atom_codes(A, Codes),
    length(Codes, Len).

:- use_module(library(tap)).

'reverse does not change length' :-
    quickcheck(prop_reverse_length/1).

'always fails'(fail) :-
    quickcheck(prop_nonsense/2).

'atom_length/2 and length/2 agree' :-
    quickcheck(prop_atom_lengths/1).
