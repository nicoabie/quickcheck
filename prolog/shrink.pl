:- use_module(library(lists)).


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

shrink(string, S, Shrunk) :-
    string_codes(S, Codes),
    shrink(codes, Codes, Codes1),
    string_codes(Shrunk, Codes1).


% help shrink lists with bisection
shrink_list_bisect(L0, L) :-
    length(L0, Len),
    Len > 0,
    MaxExponent is ceiling(log(Len)),
    between(0,MaxExponent,Exponent),
    N is round(exp(MaxExponent-Exponent)),
    shrink_list_bisect_(L0, Len, N, L).

% shrink by removing large pieces of a list
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

% shrink by removing or shrinking individual list elements
shrink_list_one(_, [], []).
shrink_list_one(Type, [H0|T], [H|T]) :-
    shrink(Type, H0, H).
shrink_list_one(Type, [H|T0], [H|T]) :-
    shrink_list_one(Type, T0, T).
