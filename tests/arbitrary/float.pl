:- begin_tests(arbitrary_float).

prop_float(F:float) :-
    is_of_type(float, F).

test('float never produces other than float') :-
  quickcheck(prop_float/1).
  
:- end_tests(arbitrary_float).