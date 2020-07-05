:- object(basics_terms).
:- public([term/4]).

term(t(Name, Derivation, Values), Name, Derivation, Values).
term_name(Term, Name) :- term(Term, Name, _, _).
term_derivation(Term, Derivation) :- term(Term, _, Derivation, _).
term_values(Term, Values) :- term(Term, _, _, Values).

:- end_object.
