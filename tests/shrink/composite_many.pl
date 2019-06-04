:- multifile quickcheck:composite/3.
quickcheck:composite(tuple(integer, string), [I:integer, S:string], X) :-
  X = (I,S).

:- begin_tests(shrink_composite_many).

prop_tuple(T:tuple(integer, string)) :-
  T = (_, S),
  string_length(S, L),
  L < 4.

test('nonsense, tuples cannot have large string', [forall(test_count_generator(_))]) :-
  catch(quickcheck(prop_tuple/1), error(counter_example, [(_, String):tuple(integer, string)]), true),
  string_length(String, Len),
  Len >= 4.

:- end_tests(shrink_composite_many).