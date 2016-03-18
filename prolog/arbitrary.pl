% define arbitrary/2 and friends

:- use_module(library(apply), [maplist/2]).

:- if(\+ predicate_property(maplist(_,  _, _),_)).

:- use_module(library(apply_macros), [maplist/3]).

:- endif.

:- use_module(library(random), [random_between/3, random_member/2, random/1]).
:- if(\+predicate_property(random_between(_, _, _), _)).

:- use_module(library(random), [random/3]).

random_between(Lo, Hi, X) :-
        random(Lo, Hi, X).

:- endif.

:- if(\+predicate_property(random_member(_, _), _)).

random_member(X, Xs) :-
        length(Xs, N),
        random_between(1, N, I),
        nth(I, Xs, X).

:- endif.


:- multifile error:has_type/2.
error:has_type(arbitrary_type, Type) :-
    nonvar(Type),
    \+ \+ quickcheck:arbitrary_type(Type).

arbitrary(any, X) :-
    setof( Type
         , ( arbitrary_type(Type)
           , ground(Type)  % exclude parameterized types
           , Type \== any  % don't recurse
           , Type \== arbitrary_type
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
    random(F), 
    X is I * F.

arbitrary(integer, X) :-
    random_between(-30000, 30000, X).

arbitrary(list, X) :-
    arbitrary(list(any), X).

arbitrary(list(T), X) :-
    random_between(1,30,Length),
    length(X, Length),
    maplist(arbitrary(T), X).

arbitrary(negative_integer, X) :-
    random_between(-30000, 1, X).

arbitrary(nonneg, X) :-
    random_between(0, 30000, X).

arbitrary(number, X) :-
    random_member(Type, [integer, float]),
    arbitrary(Type, X).

arbitrary(oneof(L), X) :-
    random_member(X, L).

arbitrary(positive_integer, X) :-
    random_between(1, 30000, X).

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


arbitrary_type(Type) :-
    clause(arbitrary(Type, _), _).


