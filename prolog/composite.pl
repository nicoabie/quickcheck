:- multifile error:has_type/2.
error:has_type(Type, Term) :-
  (clause(quickcheck:has_type(Type, _), _) -> 
    quickcheck:has_type(Type, Term)
    ;
    % verify there is a composite of the given type
    clause(composite(Type, _, _), _),
    % run the composite backwards
    composite(Type, Arbitraries, Term),
    % verify base values are of proper types
    forall(member(ArbValue:ArbType, Arbitraries), is_of_type(ArbType, ArbValue))
  ).