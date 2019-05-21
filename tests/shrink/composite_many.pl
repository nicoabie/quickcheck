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

% TODO find a way to use setting(test_count, TestCount) instead of nasty hard-coded 200.
test('nonsense, tuples cannot have large string', [forall(between(1, 200, _))]) :-
  catch(quickcheck(prop_tuple/1), error(domain_error(counter_example, [(_, String):tuple]), _), true),
  string_length(String, Len),
  Len >= 4.

:- end_tests(shrink_composite_many).