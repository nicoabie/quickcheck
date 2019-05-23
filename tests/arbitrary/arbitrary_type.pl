:- begin_tests(arbitrary_type).

test('arbitrary_type(+Type) Type does not unifies to arbitrary_type') :-
  setof(Type, ( arbitrary_type(Type)), Types),
  (
    member(arbitrary_type, Types) ->
    type_error(arbitrary, arbitrary_type)
    ;
    true
  ).
    
:- end_tests(arbitrary_type).