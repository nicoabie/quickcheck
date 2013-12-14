:- module(quickcheck, [ arbitrary/2
                      , arbitrary_type/1
                      , shrink/3
                      , quickcheck/1
                      ]).
:- use_module(library(error), [existence_error/2]).

%% arbitrary(+Type, -Value) is det.
%
%  Generate a random Value of Type. If you define your own types, add
%  clauses to this multifile predicate to support them in quickcheck.
:- multifile arbitrary/2.

%% arbitrary_type(?Type) is multi
%
%  True if Type supports arbitrary/2.

:- [arbitrary].


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

:- [shrink].


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
