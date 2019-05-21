:- multifile quickcheck:composite/3.
quickcheck:composite(even, I:integer, X) :-
  X is 2 * I.

quickcheck:composite(odd, I:integer, X) :-
  X is 2 * I + 1.

% TODO maybe it is possible to use the composite
% itself to check if hast_type
:- multifile error:has_type/2.
error:has_type(odd, X) :-
  integer(X),
  1 is X mod 2.

:- begin_tests(shrink_composite).

prop_even_numbers(E:even) :-
  E mod 2 =:= 0,
  E < 12.

% TODO find a way to use setting(test_count, TestCount) instead of nasty hard-coded 200.
test('all even numbers mod two equals zero', [forall(between(1, 200, _))]) :-
  catch(quickcheck(prop_even_numbers/1), error(domain_error(counter_example, [E:even]), _), true),
  E >= 12.

% an odd plus an even is an odd
prop_odd_plus_even(O:odd, E:even) :-
  Sum is O + E,
  is_of_type(odd, Sum),
  Sum < 4.

% TODO find a way to use setting(test_count, TestCount) instead of nasty hard-coded 200.
test('even numbers plus odd numbers gives odd', [forall(between(1, 200, _))]) :-
  catch(quickcheck(prop_odd_plus_even/2), error(domain_error(counter_example, [O:odd, E:even]), _), true),
  S is O + E,
  S >= 4.

:- end_tests(shrink_composite).