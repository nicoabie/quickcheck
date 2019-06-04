:- module(quickcheck, [ arbitrary/2
                      , arbitrary_type/1
                      , shrink/3
                      , quickcheck/1
                      ]).
:- use_module(library(error), [existence_error/2]).
:- use_module(library(settings), [setting/4, setting/2]).

% Module settings
% ---------------
:- setting( test_count, positive_integer, 100, 'Number of random test cases to generate for each test.').


%% arbitrary(+Type, -Value) is det.
%
%  Generate a random Value of Type. If you define your own types, add
%  clauses to this multifile predicate to support them in quickcheck.
%  When defining your own types, it can be helpful to call arbitrary/2
%  recursively or to use library(random).
%
%  The following types from library(error) have built in support.
%
%    * `any`
%    * `atom`
%    * `atomic`
%    * between(L,U)
%    * `boolean`
%    * `chars`
%    * `code` - printable ASCII for now
%    * `codes`
%    * `encoding`
%    * `float`
%    * `integer`
%    * `list`
%    * list(T)
%    * `negative_integer`
%    * `nonneg alias natural`
%    * `number`
%    * oneof(L)
%    * `positive_integer`
%    * `rational`
%    * `string`
%    * `text`
:- multifile arbitrary/2.

:- [arbitrary].


%% shrink(+Type, +Value, -Smaller) is nondet.
%
%  True if Smaller is a "smaller" version of Value according to the
%  semantics of Type. This predicate is called after
%  quickcheck finds a Value for which a property fails. By recursively
%  shrinking values, we can obtain a minimal, failing example.
%
%  When defining shrink/3 for your own types, be sure to fail if Value
%  cannot be shrunk any smaller. It's acceptable to produce additional
%  shrunken values on backtracking. It's often best to bisect your
%  type's values (rather than iterating all possible, smaller values) if
%  bisecting makes for your type.
:- multifile shrink/3.

:- [shrink].

%% composite(+Type, +Arbitrary:BaseType, -Value) is det.
%
%  Generate a random Value of Type from a given Arbitrary.
:- multifile composite/3.

:- [composite].

%% quickcheck(+Property:atom) is semidet.
%
%  True if Property holds for many random values. Property should be a
%  Name/Arity term. Details about test results are displayed
%  on the `user_error` stream.
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
    setting(test_count, TestCount),
    run_tests(TestCount, Module, Property, Args, Result),
    ( Result = ok ->
        warn("~d tests OK", [TestCount])
    ; Result = fail(Example) ->
        ExampleGoal =.. [Property|Example],
        warn("Failed test ~q", [ExampleGoal]),
        throw(error(counter_example, Example))
    ).

head([H|_], H).

run_tests(TestCount, Module, Property, Args, fail(Example)) :-
    between(1,TestCount,_),
    generate_arguments(Args, Values, ValuesWithBaseTypes),
    Goal =.. [Property|Values],
    \+ Module:Goal,
    !,

    % try shrinking this counter example
    shrink_example(0, Module, Property, ValuesWithBaseTypes, Example).
run_tests(_, _, _, _, ok).


% generates argument for simple arbitraries types
generate_argument(_:Type, [Value:Type]) :-
    arbitrary(Type, Value).

% generates argument for composites of one arbitrary type
generate_argument(_:Type, [Value:Type, BaseValue:BaseType]) :-
    % verify there is a composite of the given type
    clause(composite(Type, _:BaseType, _), _),
    generate_argument(_:BaseType, [BaseValue:BaseType]),
    composite(Type, BaseValue:BaseType, Value).

% generates argument for composites of many arbitraries types
generate_argument(_:Type, [Value:Type|BaseValues]) :-
    % verify there is a composite of the given type
    clause(composite(Type, [HBaseTypes|TBaseTypes], _), _),
    generate_arguments([HBaseTypes|TBaseTypes], BaseValues, _),
    composite(Type, BaseValues, Value).

generate_arguments(Args, Values, ValuesWithBaseTypes) :-
    maplist(generate_argument, Args, ValuesWithBaseTypes),
    maplist(head, ValuesWithBaseTypes, Values).


% shrink a typed argument
shrink_argument(Value:Type, [Shrunken:Type]) :-
    shrink(Type, Value, Shrunken).
shrink_argument([Value:Type], [Shrunken:Type]) :-
    shrink(Type, Value, Shrunken).

% shrink a composite argument
% we are not interested in the current value
% we need to generate shrunken values of the base
% arbitraries and then make a composite
shrink_argument([_:Type|BaseValuesWithTypes], [Shrunken:Type|ShrunkenBaseValuesWithTypes]) :-
    % verify there is a composite of the given type
    clause(composite(Type, BaseValuesWithTypes, _), _),
    shrink_arguments(BaseValuesWithTypes, ShrunkenBaseValuesWithTypes, _),
    composite(Type, ShrunkenBaseValuesWithTypes, Shrunken).

% there is no shrinker for the given Type
% return the same Value as shrunken
shrink_argument([Value:Type|T], [Value:Type|T]).

shrink_arguments(ValuesWithBaseTypes, Shrunk, ShrunkWithBaseTypes) :-
    maplist(shrink_argument, ValuesWithBaseTypes, ShrunkWithBaseTypes),
    maplist(head, ShrunkWithBaseTypes, Shrunk).


shrink_example(Depth0, Module, Property, ValuesWithBaseTypes, Example) :-
    Depth0 < 32,
    shrink_arguments(ValuesWithBaseTypes, Shrunk, ShrunkWithBaseTypes),
    ValuesWithBaseTypes \== ShrunkWithBaseTypes,
    ShrinkGoal =.. [Property|Shrunk],
    \+ Module:ShrinkGoal,
    !,
    Depth is Depth0 + 1,
    shrink_example(Depth, Module, Property, ShrunkWithBaseTypes, Example).

shrink_example(Depth,_,_,ValuesWithBaseTypes, Example) :-
    warn("Shrinking to depth ~d", [Depth]),
    % omit base types in example
    maplist(head, ValuesWithBaseTypes, Example).


:- dynamic tap_raw:is_test_running/0, tap_raw:diag/2.

warn(Format,Args) :-
    current_module(tap_raw),
    tap_raw:is_test_running,
    !,
    tap_raw:diag(Format,Args).
warn(Format,Args) :-
    format(user_error,Format,Args),
    writeln('').
    
