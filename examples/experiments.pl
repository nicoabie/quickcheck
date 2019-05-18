composite(even, I:integer, X) :-
  X is 2 * I.

replace_firsts_with_elems([], [], []).

replace_firsts_with_elems([HL|TL], [HE|TE], R) :-
  HL = [_|THL],
  R = [[HE|THL]|RR],
  replace_firsts_with_elems(TL, TE, RR).
