:- use_module(prolog/quickcheck).
:- begin_tests(arbitrary_negative_integer).

prop_negative_integer(I:negative_integer) :-
  I < 0. 

prop_negative_integer_type(I:negative_integer) :-
  is_of_type(integer, I).
  
test('negative integer must be strictly lower than zero') :-
  quickcheck(prop_negative_integer/1).

test('negative integer never produces other than integer') :-
  quickcheck(prop_negative_integer_type/1).

:- end_tests(arbitrary_negative_integer).