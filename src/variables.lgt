:- object(variables).

:- public([variable_calculation/4, variable_value/3, variable_value/4, useless_variable/1]).

/* variable_calculation(Variable, Variables, Calculations, Calculation)
*/

variable_calculation(Variable, [Variable|_], [Calculation|_], Calculation).

variable_calculation(Variable, [_|Variables], [_|Calculations], Calculation) :-
          variable_calculation(Variable, Variables, Calculations, Calculation).


/* variable_value(Variable, Variables, Values, Value)
*/

variable_value(Variable, [Variable|_], [Value|_], Value).

variable_value(Variable, [_|Variables], [_|Values], Value) :-
          variable_value(Variable, Variables, Values, Value).


useless_variable(monotonic(Type, X, Y)) :-
          useless_variable1(Type, X, Y) -> true
          ;
          useless_variable1(Type, Y, X) -> true
          ;
          fail.


useless_variable1(decreasing, X, monotonic(increasing, _, X)).

useless_variable1(increasing, X, monotonic(decreasing, X, _)).

useless_variable1(increasing, X, monotonic(decreasing, _, X)).

useless_variable1(increasing, X, monotonic(increasing, X,_)).

useless_variable1(decreasing, monotonic(increasing, _,Y), monotonic(increasing, Y, _)).

useless_variable1(decreasing, monotonic(increasing, X,_), monotonic(increasing, _, X)).

useless_variable1(increasing, monotonic(decreasing, W,X), monotonic(decreasing, Y, Z)) :-
          intersect::intersect([W,X], [Y,Z], Intersection),
          Intersection \= [].

:- end_object.