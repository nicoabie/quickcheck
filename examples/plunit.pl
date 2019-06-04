:- use_module(library(quickcheck)).

prop_reverse_twice(L:list) :-
    reverse(L, R),
    reverse(R, L).

prop_silly_list(L:list(integer)) :-
    length(L, Len),
    Len =:= 3.  % nonsense!

:- begin_tests(list).

test('reverse twice') :-
    quickcheck(prop_reverse_twice/1).

test('silly list', [error(_)]) :-
    quickcheck(prop_silly_list/1).

:- end_tests(list).
