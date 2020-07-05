:- object(reduce_experiments).

:- public([reduce_experiments/4]).

reduce_experiments([], _,_, []).

reduce_experiments([Variable|OtherVariables], Experiments,
                   OldVariables,  [NewValue|OtherNewValues]) :-
          evaluate_law(Variable, Experiments, OldVariables,  NewValue),
          !,
          reduce_experiments(OtherVariables, Experiments, OldVariables, OtherNewValues).


evaluate_law(constant(Variable), Experiments, OldVariables, NewValue) :-
          calculate_constant(Variable, Experiments, OldVariables, NewValue).

evaluate_law(slope(Variable1, Variable2), Experiments, OldVariables,  NewValue) :-
          calculate_slope(Variable1, Variable2, Experiments, OldVariables,  NewValue).

evaluate_law(intercept(Variable1, Variable2), Experiments, OldVariables,  NewValue) :-
          calculate_intercept(Variable1, Variable2, Experiments, OldVariables,  NewValue).


calculate_constant(Variable, Experiments, OldVariables, NewValue) :-
          calculate_constant(Variable, 0, 0, Experiments, OldVariables, Count, Sum),
          NewValue is Sum / Count.


calculate_constant(_, Count, SimpleSum, [], _, Count, SimpleSum).

calculate_constant(Variable, CountIn, SimpleSumIn,
                   [Experiment|OtherExperiments], OldVariables, CountOut, SimpleSumOut) :-
          evaluate_variable(Variable, OldVariables, Experiment, Value),
          CountNext is CountIn + 1,
          SimpleSumNext is SimpleSumIn + Value,
          calculate_constant(Variable, CountNext, SimpleSumNext, OtherExperiments,
                             OldVariables, CountOut, SimpleSumOut).


calculate_slope(Variable1, Variable2, Experiments, OldVariables, Slope) :-
          calculate_linear(Variable1, Variable2, Experiments, OldVariables, Slope, _).


calculate_linear(Variable1, Variable2, Experiments, OldVariables, Slope, Intercept) :-
          extract_pair(Experiments, Variable1, Variable2,OldVariables,  [Pair|OtherPairs]),
          pairwise_expansion::pairwise_expansion(Pair, ExpandedPair),
          infer_linear::check_linear(OtherPairs, ExpandedPair, [Evaluation]),
          calculate_linear_evaluation(Evaluation, Slope, Intercept).


calculate_linear_evaluation(linear(Count,Squares1,Squares2,Simple1, Simple2, Product),
                            Slope, Intercept) :-
          infer_linear::linear_approximation(Count,Squares1, Squares2, Simple1, Simple2,
                               Product, _, _, Slope, Intercept, _).


extract_pair([], _, _, _,  []).

extract_pair([Experiment|OtherExperiments], Variable1, Variable2,
             OldVariables, [[Value1,Value2]|OtherPairs]) :-
          evaluate_variable(Variable1,  OldVariables, Experiment, Value1),
          evaluate_variable(Variable2,  OldVariables, Experiment, Value2),
          extract_pair(OtherExperiments, Variable1, Variable2, OldVariables, OtherPairs).


calculate_intercept(Variable1, Variable2, [Experiment1, Experiment2|_],
                    OldVariables, Value) :-
          evaluate_variable(Variable1,  OldVariables, Experiment1, X1),
          evaluate_variable(Variable2,  OldVariables, Experiment1, Y1),
          evaluate_variable(Variable1,  OldVariables, Experiment2, X2),
          evaluate_variable(Variable2,  OldVariables, Experiment2, Y2),
          Slope is (Y1 - Y2) / (X1 - X2),
          Value is Y1 - X1 * Slope.


evaluate_variable(Variable, OldVariables, Experiment, Value) :-
          Variable = monotonic(Type, Variable1, Variable2)
            -> calculate_monotonic(Type, Variable1, Variable2, OldVariables, Experiment, Value)
          ;
          variables::variable_value(Variable, OldVariables, Experiment, Value).


calculate_monotonic(increasing, Variable1, Variable2, OldVariables, Experiment, Value) :-
          evaluate_variable(Variable1, OldVariables, Experiment, Value1),
          evaluate_variable(Variable2, OldVariables, Experiment, Value2),
          Value is Value1 / Value2.

calculate_monotonic(decreasing, Variable1, Variable2, OldVariables, Experiment, Value) :-
          evaluate_variable(Variable1, OldVariables, Experiment, Value1),
          evaluate_variable(Variable2, OldVariables, Experiment, Value2),
          Value is Value1 * Value2.


:- end_object.
