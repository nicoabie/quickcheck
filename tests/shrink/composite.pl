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

% an odd plus an even is an odd
prop_odd_plus_even(O:odd, E:even) :-
  Sum is O + E,
  is_of_type(odd, Sum),
  Sum < 4.

% TODO assert shrink counter example
test('even numbers plus odd numbers gives odd', fail) :-
  quickcheck(prop_odd_plus_even/2).

prop_even_numbers(E:even) :-
  E mod 2 =:= 0,
  E < 4.

% TODO assert shrink counter example
test('all even numbers mod two equals zero', fail) :-
  quickcheck(prop_even_numbers/1).

:- end_tests(shrink_composite).