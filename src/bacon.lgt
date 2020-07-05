:- object(bacon).
:- public([bacon/5]).

bacon(IndependentVariables, DependentVariables, IndependentTerms, ResultingDependentTerms, Laws) :-
  initial_data(IndependentVariables, DependentVariables, IndependentTerms, InitialDependentTerms),
  find_laws::find_laws(IndependentTerms, InitialDependentTerms, ResultingDependentTerms, Laws).

initial_data(IndependentVariables, DependentVariables, IndependentTerms, DependentTerms) :-
  variables_to_terms(IndependentVariables, independent, IndependentTerms),
  variables_to_terms(DependentVariables, dependent, DependentTerms).

variables_to_terms([], _, []).
variables_to_terms([Name-Values|OtherVariables], Derivation, [Term|OtherTerms]) :-
  basics_terms::term(Term, Name, Derivation, Values),
  variables_to_terms(OtherVariables, Derivation, OtherTerms).

:- end_object.
