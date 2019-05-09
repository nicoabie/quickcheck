:- use_module(prolog/quickcheck).

prop_integer(I:integer) :-
  integer(I).

prop_nonsense(A:list(integer), B:list(integer)) :-
  length(A, LenA),
  length(B, LenB),
  LenA =< LenB.

:- use_module(library(tap)).

'always succeeds' :-
  quickcheck(prop_integer/1).

'always fails'(fail) :-
  quickcheck(prop_nonsense/2).