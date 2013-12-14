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
arbitrary(atomic, X) :-
    random_member(Type, [atom,float,integer,string]),
    arbitrary(Type, X).
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
shrink(any, X, X).

shrink(atom, Atom, Shrunk) :-
    atom_codes(Atom, Codes0),
    shrink(codes, Codes0, Codes),
    atom_codes(Shrunk, Codes).

shrink(code, _, 0'a).
shrink(code, _, 0'b).
shrink(code, _, 0'c).
shrink(code, _, 0' ).

shrink(codes, Codes0, Codes) :-
    shrink(list(code), Codes0, Codes).

shrink(integer, _, 0).  % zero often triggers bugs
shrink(integer, X, Y) :-
    % bisect from 1 towards the integer
    X > 0,
    MaxExponent is floor(log(abs(X))),
    between(0,MaxExponent,Exponent),
    Y is sign(X) * round(exp(Exponent)).
shrink(integer, X, Y) :-
    % try a positive version of a negative integer
    X < 0,
    Y is -X.

shrink(list, L0, L) :-
    shrink(list(any), L0, L).

shrink(list(_), L0, L) :-
    shrink_list_bisect(L0, L).
shrink(list(Type), L0, L) :-
    shrink_list_one(Type, L0, L).

shrink(string, _, X) :-
    shrink(codes, _, Codes),
    string_codes(X, Codes).


% help shrink lists with bisection
shrink_list_bisect(L0, L) :-
    length(L0, Len),
    Len > 0,
    MaxExponent is ceil(log(Len)),
    between(0,MaxExponent,Exponent),
    N is round(exp(MaxExponent-Exponent)),
    shrink_list_bisect_(L0, Len, N, L).

shrink_list_bisect_([], _, _, []).
shrink_list_bisect_(_, Len, N, []) :-
    N > Len.
shrink_list_bisect_(L0, Len, N, L) :-
    length(Front, N),
    append(Front, Back, L0),
    ( L = Back
    ; BackLen is Len - N,
      shrink_list_bisect_(Back, BackLen, N, NewBack),
      append(Front, NewBack, L)
    ).


shrink_list_one(_, [], []).
shrink_list_one(Type, [H0|T], [H|T]) :-
    shrink(Type, H0, H).
shrink_list_one(Type, [H|T0], [H|T]) :-
    shrink_list_one(Type, T0, T).


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
    TestCount = 100,
    run_tests(TestCount, Module, Property, Args, Result),
    ( Result = ok ->
        debug(quickcheck, "~d tests OK", [TestCount])
    ; Result = fail(Example) ->
        ExampleGoal =.. [Property|Example],
        debug(quickcheck, "Failed test ~q", [ExampleGoal]),
        fail
    ).


run_tests(TestCount, Module, Property, Args, fail(Example)) :-
    between(1,TestCount,_),
    maplist(generate_argument, Args, Values),
    Goal =.. [Property|Values],
    \+ Module:call(Goal),
    !,

    % try shrinking this counter example
    shrink_example(0, Module, Property, Values, Example).
run_tests(_, _, _, _, ok).



% separate a property argument into a variable and a type
generate_argument(_:Type, Value:Type) :-
    arbitrary(Type, Value).


% shrink a typed argument
shrink_argument(Value:Type, Shrunken:Type) :-
    shrink(Type, Value, Shrunken).
shrink_argument(Value:Type, Value:Type).


shrink_example(Depth0, Module, Property, Values, Example) :-
    Depth0 < 32,
    maplist(shrink_argument, Values, Shrunk),
    Values \== Shrunk,
    ShrinkGoal =.. [Property|Shrunk],
    \+ Module:call(ShrinkGoal),
    !,
    Depth is Depth0 + 1,
    shrink_example(Depth, Module, Property, Shrunk, Example).
shrink_example(Depth,_,_,Example, Example) :-
    debug(quickcheck, "Shrinking to depth ~d", [Depth]).
