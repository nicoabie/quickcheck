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
%    * `nonneg`
%    * `number`
%    * oneof(L)
%    * `positive_integer`
%    * `rational`
%    * `string`
%    * `text`
:- multifile arbitrary/2.

%% arbitrary_type(?Type) is multi
%
%  True if Type supports arbitrary/2.

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
    warn("Shrinking to depth ~d", [Depth]).


:- dynamic tap_raw:diag/2.

warn(Format,Args) :-
    current_module(tap_raw),
    !,
    tap_raw:diag(Format,Args).
warn(Format,Args) :-
    format(user_error,Format,Args),
    format(user_error,"~n",[]).
