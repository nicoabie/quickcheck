:- use_module('../prolog/quickcheck.pl').
:- use_module(library(settings)).

:- set_setting(quickcheck:test_count, 10000). 

prop_negative_integer(I:negative_integer) :-
    I < 0. 

:- use_module(library(tap)).

'negative integer must be strictly lower than zero' :-
  quickcheck(prop_negative_integer/1).
