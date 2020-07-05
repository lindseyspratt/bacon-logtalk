:- object(interesting_regularities).
:- public([interesting_regularities/5, depends_on/2]).

interesting_regularities([IndependentVariable|_], ValuesIn, VariablesIn,
                         ValuesOut, VariablesOut) :-
           interesting_regularities1(ValuesIn, VariablesIn, IndependentVariable,
                                     ValuesOut, VariablesOut).


interesting_regularities1([], [], _,  [], []).

interesting_regularities1([ValueIn|OtherValuesIn],
                          [VariableIn|OtherVariablesIn], IndependentVariable,
                          ValuesOut, VariablesOut) :-
          (depends_on(VariableIn, IndependentVariable)
            -> VariablesOut = [VariableIn|OtherVariablesOut],
               ValuesOut = [ValueIn|OtherValuesOut]
          ;
           VariablesOut = OtherVariablesOut,
           ValuesOut = OtherValuesOut
          ),
          interesting_regularities1(OtherValuesIn, OtherVariablesIn,
                                    IndependentVariable, OtherValuesOut, OtherVariablesOut).


depends_on(Variable, Variable) :- !.

depends_on(Composite, Element) :-
          Composite =.. [_|Arguments],
          depends_on_list(Arguments, Element).


depends_on_list([Argument|_], Element) :-
          depends_on(Argument, Element),
          !.

depends_on_list([_|Arguments], Element) :-
          depends_on_list(Arguments, Element).

:- end_object.
