:- multifile quickcheck:composite/3.
quickcheck:composite(vector(integer, integer), [A:integer, B:integer], X) :-
  X = (A,B).

:- begin_tests(composite_many_arguments).

prop_vector_sum((A1, A2):vector(integer, integer), (B1, B2):vector(integer, integer)) :-
  C1 is A1 + B1,
  C2 is A2 + B2,
  C = (C1, C2),
  is_of_type(vector(integer, integer), C).

test('vectors can be summed') :-
  quickcheck(prop_vector_sum/2).

:- end_tests(composite_many_arguments).