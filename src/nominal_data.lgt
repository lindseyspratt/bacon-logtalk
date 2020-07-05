:- object(nominal_data).
:- public([nominal_data/1]).

nominal_data(Term) :-
  \+ number(Term).

:- end_object.
