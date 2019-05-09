:- use_module(prolog/quickcheck).
:- begin_tests(arbitrary_float).

prop_float(F:float) :-
    float(F). 

test('float never produces other than float') :-
  quickcheck(prop_float/1).
  
:- end_tests(arbitrary_float).