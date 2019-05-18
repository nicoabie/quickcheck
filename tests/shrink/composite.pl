% :- multifile quickcheck:arbitrary/2.
% quickcheck:arbitrary(even, X) :-
%   arbitrary(interger, I),
%   X is 2 * I.

:- multifile quickcheck:composite/3.
quickcheck:composite(even, I:integer, X) :-
  X is 2 * I.

:- begin_tests(shrink_composite).

prop_even_numbers(E:even) :-
  E mod 2 =:= 0,
  E < 4. 

test('all even numbers mod two equals zero') :-
  quickcheck(prop_even_numbers/1).

:- end_tests(shrink_composite).