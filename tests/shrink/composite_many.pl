:- multifile quickcheck:composite/3.
quickcheck:composite(tuple, [I:integer, S:string], X) :-
  X = (I,S).

:- begin_tests(shrink_composite_many).

prop_tuple(T:tuple) :-
  T = (_, S),
  string_length(S, L),
  L < 4.

% TODO find a way to use setting(test_count, TestCount) instead of nasty hard-coded 200.
test('nonsense, tuples cannot have large string', [forall(between(1, 200, _))]) :-
  catch(quickcheck(prop_tuple/1), error(counter_example, [(_, String):tuple]), true),
  string_length(String, Len),
  Len >= 4.

prop_tuple_sum((AI, AS):tuple, (BI, BS):tuple) :-
  CI is AI + BI,
  string_concat(AS, BS, CS),
  TS = (CI, CS),
  is_of_type(tuple, TS).

test('tuples can be summed') :-
  quickcheck(prop_tuple_sum/2).

:- end_tests(shrink_composite_many).