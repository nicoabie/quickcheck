:- multifile quickcheck:composite/3.
quickcheck:composite(even, I:integer, X) :-
  X is 2 * I.

:- begin_tests(composite_single_argument).

prop_even_numbers(E:even) :-
  E mod 2 =:= 0.

test('all even numbers mod two equals zero') :-
  quickcheck(prop_even_numbers/1).

:- end_tests(composite_single_argument).