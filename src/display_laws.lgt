:- object(display_laws).
:- public([display_laws/2]).

display_laws(Variables, Values) :-
          Variables = TopVariables - _OtherVariables
            -> display_laws1(TopVariables, Values)
          ;
          display_laws1(Variables, Values).


display_laws1([], []).

display_laws1([Variable|OtherVariables], [Value|OtherValues]) :-
          simplify::simplify(Variable, SimpleVariable),
          write(SimpleVariable),
          write(' = '),
          write(Value),
          nl,
          display_laws1(OtherVariables, OtherValues).

:- end_object.