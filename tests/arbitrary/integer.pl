:- begin_tests(arbitrary_integer).

prop_integer(I:integer) :-
    is_of_type(integer, I),
    I < 4.

test('integer never produces other than integer') :-
  quickcheck(prop_integer/1).
  
:- end_tests(arbitrary_integer).