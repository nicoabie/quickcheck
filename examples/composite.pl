:- use_module(library(quickcheck)).
% this is the way of creating new arbitraries
% defining a composite that will receive a list of primary arbitraries.
:- multifile quickcheck:composite/3.
quickcheck:composite(tuple(integer, string), [I:integer, S:string], X) :-
  X = (I,S).

% composites can also be defined using a single arbitrary 
% without need to passing a list
quickcheck:composite(even, I:integer, X) :-
  X is 2 * I.

quickcheck:composite(odd, I:integer, X) :-
  X is 2 * I + 1.

% once we define a composite, we may need to define
% a has_type/2 in case we are going to make use of 
% the predicate is_of_type(+Composite, @Term).
% in the case where the composite body is 'invertible' 
% (ie: tuple(integer, string)) there is no need to
% because quickcheck will run it 'backwards'
% but in cases such as odd where the body is not 'invertible'
% (meaning I cannot be a variable, so it can be run 'backwards')
% a custom definition must be given.
:- multifile quickcheck:has_type/2.
quickcheck:has_type(odd, X) :-
  integer(X),
  1 is X mod 2.

prop_tuple(T:tuple(integer, string)) :-
  T = (_, S),
  string_length(S, L),
  L < 4.

prop_odd_plus_even(O:odd, E:even) :-
  Sum is O + E,
  is_of_type(odd, Sum),
  Sum < 4.

:- begin_tests(composite).

test('nonsense, tuples cannot have large string', [forall(between(1, 10, _))]) :-
  catch(quickcheck(prop_tuple/1), error(counter_example, [(_, String):tuple(integer, string)]), true),
  string_length(String, Len),
  Len >= 4.

test('even numbers plus odd numbers gives odd', [forall(between(1, 10, _))]) :-
  catch(quickcheck(prop_odd_plus_even/2), error(counter_example, [O:odd, E:even]), true),
  S is O + E,
  S >= 4.

:- end_tests(composite).