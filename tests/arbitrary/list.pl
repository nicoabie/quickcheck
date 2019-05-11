:- use_module(prolog/quickcheck).
:- begin_tests(arbitrary_list).

prop_list_string(Ls:list(string)) :-
    is_of_type(list(string), Ls).

test('list(string) never produces other than list(string)') :-
  quickcheck(prop_list_string/1).
  
:- end_tests(arbitrary_list).