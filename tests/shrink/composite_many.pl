:- multifile quickcheck:composite/3.
quickcheck:composite(tuple, [I:integer, S:string], X) :-
  X = (I,S).

% TODO maybe it is possible to use the composite
% itself to check if hast_type
:- multifile error:has_type/2.
error:has_type(tuple, X) :-
  X = (I,S),
  integer(I),
  string(S).

:- begin_tests(shrink_composite_many).

prop_tuple(T:tuple) :-
  T = (_, S),
  string_length(S, L),
  L < 4.

% TODO assert shrink counter example
test('nonsense, tuples cannot have large string', fail) :-
  quickcheck(prop_tuple/1).

:- end_tests(shrink_composite_many).