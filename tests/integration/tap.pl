:- use_module(library(prolog_pack)).

:- if(prolog_pack:current_pack(tap)).

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

:- else.
  % if tap is not currently installed
  % create an empty main goal.
  main.
:- endif.
