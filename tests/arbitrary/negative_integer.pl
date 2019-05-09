:- use_module(prolog/quickcheck).
:- begin_tests(arbitrary_negative_integer).

prop_negative_integer(I:negative_integer) :-
    I < 0. 

test('negative integer must be strictly lower than zero') :-
  quickcheck(prop_negative_integer/1).
  
:- end_tests(arbitrary_negative_integer).