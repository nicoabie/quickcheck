% Tests extracted from quickcheck tutorial posted on
% blog.ndrix.com
:- use_module(library(quickcheck)).
:- use_module(library(error)).

% intentionally left with slow 'list' type to match tutorial
prop_reverse_twice(L:list) :-
    reverse(L, R),
    reverse(R, L).


prop_silly_list(L:list(integer)) :-
    length(L, Len),
    Len =:= 3.  % nonsense!


:- multifile error:has_type/2.
error:has_type(even, X) :-
    integer(X),
    0 is X mod 2.
error:has_type(odd, X) :-
    integer(X),
    1 is X mod 2.


:- multifile quickcheck:arbitrary/2.
quickcheck:arbitrary(even, X) :-
    arbitrary(integer, I),
    X is 2*I.
quickcheck:arbitrary(odd, X) :-
    arbitrary(integer, I),
    X is 2*I + 1.


% an odd plus an even is an odd
prop_odd_plus_even(O:odd, E:even) :-
    Sum is O + E,
    is_of_type(odd, Sum).


:- use_module(library(tap)).

'reverse twice' :-
    quickcheck(prop_reverse_twice/1).

'silly list'(fail) :-
    quickcheck(prop_silly_list/1).
