:- module(quickcheck, [ arbitrary/2
                      , arbitrary_type/1
                      , shrink/3
                      , quickcheck/1
                      ]).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(error), [existence_error/2]).
:- use_module(library(random), [random_between/3, random_member/2]).

:- multifile error:has_type/2.
error:has_type(arbitrary_type, Type) :-
    nonvar(Type),
    \+ \+ quickcheck:arbitrary_type(Type).

%% arbitrary(+Type, -Value) is det.
%
%  Generate a random Value of Type. If you define your own types, add
%  clauses to this multifile predicate to support them in quickcheck.
:- multifile arbitrary/2.
arbitrary(any, X) :-
    setof( Type
         , ( arbitrary_type(Type)
           , ground(Type)  % exclude parameterized types
           , Type \== any  % don't recurse
           )
         , Types
         ),
    random_member(Type, Types),
    arbitrary(Type, X).
arbitrary(atom, X) :-
    arbitrary(codes, Codes),
    atom_codes(X, Codes).
arbitrary(between(L,U), X) :-
    random_between(L,U,X).
arbitrary(boolean, X) :-
    random_member(X, [true, false]).
arbitrary(chars, X) :-
    arbitrary(atom, Atom),
    atom_chars(Atom, X).
arbitrary(code, X) :-
    random_between(0x20, 0x7e, X).  % printable ASCII
arbitrary(codes, X) :-
    arbitrary(list(code), X).
arbitrary(encoding, X) :-
    setof(E, error:current_encoding(E), Encodings),
    random_member(X, Encodings).
arbitrary(float, X) :-
    arbitrary(integer, I),
    X is I * random_float.
arbitrary(integer, X) :-
    random_between(-30_000, 30_000, X).
arbitrary(list, X) :-
    arbitrary(list(any), X).
arbitrary(list(T), X) :-
    random_between(1,30,Length),
    length(X, Length),
    maplist(arbitrary(T), X).
arbitrary(negative_integer, X) :-
    random_between(-30_000, 1, X).
arbitrary(nonneg, X) :-
    random_between(0, 30_000, X).
arbitrary(number, X) :-
    random_member(Type, [integer, float]),
    arbitrary(Type, X).
arbitrary(oneof(L), X) :-
    random_member(X, L).
arbitrary(positive_integer, X) :-
    random_between(1, 30_000, X).
arbitrary(rational, X) :-
    arbitrary(integer, Numerator),
    arbitrary(integer, Denominator),
    X is Numerator rdiv Denominator.
arbitrary(string, X) :-
    arbitrary(codes, Codes),
    string_codes(X, Codes).
arbitrary(text, X) :-
    random_member(Type, [atom, string, chars, codes]),
    arbitrary(Type, X).


%% arbitrary_type(?Type) is multi
%
%  True if Type supports arbitrary/2.
arbitrary_type(Type) :-
    clause(arbitrary(Type, _), _).


%% shrink(+Type, +Value, -Smaller) is nondet.
%
%  True if Smaller is a "smaller" version of Value according to the
%  semantics of Type. This predicate is called after
%  quickcheck finds a Value for which a property fails. By shrinking
%  values, we can obtain a more minimal test case.
%
%  When defining shrink/3 for your own types, be sure to fail if Value
%  cannot be shrunk any smaller. It's acceptable to produce additional
%  shrunken values on backtracking.
:- multifile shrink/3.
shrink(_, X, Y) :- % integral types
    integer(X),
    Y is sign(X).
shrink(atom, _, X) :-
    shrink(codes, _, Codes),
    atom_codes(X, Codes).
shrink(code, _, 0'a).
shrink(code, _, 0'b).
shrink(code, _, 0'c).
shrink(code, _, 0' ).
shrink(codes, _, []).
shrink(codes, _, [Code]) :-
    shrink(code, _, Code).
shrink(list, _, []).
shrink(list, [_|T], T).
shrink(list, [H|T0], [H|T]) :-
    shrink(list, T0, T).
shrink(list(_), _, []).
shrink(list(_), [_|T], T).
shrink(list(Type), [H0|T0], [H|T]) :-
    ( shrink(Type, H0, H)
    ; H = H0
    ),
    shrink(list(Type), T0, T).
shrink(string, _, X) :-
    shrink(codes, _, Codes),
    string_codes(X, Codes).


%% quickcheck(+Property:atom) is semidet.
%
%  True if Property holds for many random values.
:- meta_predicate quickcheck(:).
quickcheck(Module:Property/Arity) :-
    % make sure the property predicate exists
    ( Module:current_predicate(Property/Arity) ->
        true
    ; % property predicate missing ->
        existence_error(predicate, Module:Property/Arity)
    ),

    % what type is each argument?
    functor(Head, Property, Arity),
    once(Module:clause(Head, _)),
    Head =.. [Property|Args],

    % run randomized tests
    run_tests(100, Module, Property, Args).


% TODO rewrite run_tests/4
% it's ugly, hard to understand, uses a failure-driven loop
% and doesn't nicely return a single status summarizing
% whether the property holds.
run_tests(0,_,_,_) :-
    !.
run_tests(N, Module, Property, Args) :-
    maplist(generate_arguments, Args, Values),
    Goal =.. [Property|Values],
    ( Module:call(Goal) ->
        debug(quickcheck, "ok ~d", [N])
    ; % randomized test failed ->
        debug(quickcheck, "failed ~d with ~q", [N,Goal])
    ),
    fail.
run_tests(N0, Module, Property, Args) :-
    succ(N, N0),
    run_tests(N, Module, Property, Args).


% separate a property argument into a variable and a type
generate_arguments(_:Type, Value:Type) :-
    arbitrary(Type, Value).
