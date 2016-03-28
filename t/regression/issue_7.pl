/*
  https://github.com/mndrix/quickcheck/issues/7

  `quickcheck:shrink(string, "", X)` throws an exception on backtracking.
*/

:- use_module(library(quickcheck)).


:- use_module(library(tap)).

'quickcheck:shrink(string, "", X) does not throw an exception on backtracking' :-
    findall(_,
            quickcheck:shrink(string, "", _),
            _).


