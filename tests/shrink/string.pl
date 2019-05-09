:- use_module(prolog/quickcheck).
:- begin_tests(shrink_string).

test('quickcheck:shrink(string, "", X) does not throw an exception on backtracking') :-
  findall(_, quickcheck:shrink(string, "", _), _).

:- end_tests(shrink_string).