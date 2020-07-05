:- object(consider_sequence).

:- public([consider_sequence/10]).

consider_sequence(Experiments, VariablesIn, CalculationsIn, TotalVariables,
                  TotalCalculations, ValuesOut, VariablesOut, CalculationsOut,
                  SequenceVariables, SequenceCalculations) :-
          consider_sequence1(Experiments, VariablesIn, TotalVariables,
                             TotalCalculations, NewExperiments, SequenceVariables,
                             SequenceCalculations)
            -> consider_monotonic::extend_test_values(Experiments, VariablesIn, CalculationsIn, SequenceVariables,
                                  SequenceCalculations, NewExperiments, ValuesOut,
                                  VariablesOut, CalculationsOut)
          ;
          Experiments = ValuesOut,
          VariablesIn = VariablesOut,
          CalculationsIn = CalculationsOut,
          SequenceVariables = [],
          SequenceCalculations = [].


consider_sequence1(ValuesIn, VariablesIn,
                   TotalVariables, TotalCalculations,
                   NewExperiments, SequenceVariables, SequenceCalculations) :-
          ValuesIn = [Experiment|OtherExperiments],
          pairwise_expansion::pairwise_expansion(Experiment, ExpandedExperiment),
          check_sequence(OtherExperiments, ExpandedExperiment, Evaluations,
                         OtherExpandedExperiments),
          pairwise_expansion::pairwise_expansion(VariablesIn, ExpandedVariables),
          consider_sequence2(Evaluations, ExpandedVariables, TotalVariables,
                             TotalCalculations, _, [], [], Mask,
                             SequenceVariables, SequenceCalculations),
          sequence_extend_values([ExpandedExperiment|OtherExpandedExperiments],
                                 SequenceVariables, SequenceCalculations,
                                 Mask, NewExperiments),
          !. /* semantic */


consider_sequence2(Evaluations, ExpandedVariables, TotalVariables, TotalCalculations,
                   MaskIn, SequenceVariablesIn, SequenceCalculationsIn,
                   MaskOut, SequenceVariablesOut, SequenceCalculationsOut) :-
          analyze_sequence_evaluations(Evaluations, ExpandedVariables,
                                       TotalVariables, TotalCalculations,
                                       SequenceVariablesNext, SequenceCalculationsNext,
                                       MaskLocal
                                      ),
          SequenceVariablesNext \= SequenceVariablesIn,
          (SequenceVariablesNext = []
             -> _ = SequenceCalculationsNext,
                _ = MaskLocal,
                SequenceCalculationsOut = SequenceCalculationsIn,
                SequenceVariablesOut = SequenceVariablesIn,
                MaskOut = MaskIn
           ; _ = SequenceVariablesIn,
            _ = SequenceCalculationsIn,
            reevaluate_sequences(Evaluations, ExpandedVariables,
                                 SequenceVariablesNext, MaskLocal, MaskIn,
                                 NewEvaluations, NewExpandedVariables, MaskNext),
            consider_sequence2(NewEvaluations, NewExpandedVariables,
                               TotalVariables, TotalCalculations,
                               MaskNext, SequenceVariablesNext, SequenceCalculationsNext,
                               MaskOut, SequenceVariablesOut, SequenceCalculationsOut)
          ).


check_sequence([], Values, Values, []).

check_sequence([Experiment|OtherExperiments], ValuesIn, ValuesOut,
               [ExpandedExperiment|OtherExpandedExperiments]) :-
          pairwise_expansion::pairwise_expansion(Experiment, ExpandedExperiment),
          experiment_has_possibly_positional_values(ExpandedExperiment),
          check_experiment_sequence(ExpandedExperiment, ValuesIn, ValuesNext),
          check_sequence(OtherExperiments, ValuesNext, ValuesOut, OtherExpandedExperiments).


experiment_has_possibly_positional_values([Position-_|_]) :-
          \+ nominal_data::nominal_data(Position).


check_experiment_sequence([], [], []).

check_experiment_sequence([TestValue|OtherTestValues], [CheckValueIn|OtherCheckValuesIn],
                          [CheckValueOut|OtherCheckValuesOut]) :-
          accumulate_sequence(TestValue, CheckValueIn, CheckValueOut),
          check_experiment_sequence(OtherTestValues, OtherCheckValuesIn, OtherCheckValuesOut).


accumulate_sequence(Position-Value, sequence(SortedPairsIn), sequence(SortedPairsOut)) :-
          !, /* performance */
          insert_sequence(Position, Value, SortedPairsIn, SortedPairsOut).

accumulate_sequence(Position1-Value1, Position0-Value0, sequence(SortedPairs)) :-
          insert_sequence(Position1, Value1, [Position0-Value0], SortedPairs).


insert_sequence(PositionIn, ValueIn, [], [PositionIn-ValueIn]).

insert_sequence(PositionIn, ValueIn, [Position-Value|OtherPairs], PairsOut) :-
          PositionIn < Position
            -> PairsOut = [PositionIn-ValueIn, Position-Value|OtherPairs]
          ;
           PairsOut = [Position-Value|OtherPairsOut],
           insert_sequence(PositionIn, ValueIn, OtherPairs, OtherPairsOut).


%analyze_sequence_evaluations([], [], OldVariables, OldCalculations, OldVariables,
%                             OldCalculations, []).

analyze_sequence_evaluations([], [], _, _, [],
                             [], []).

analyze_sequence_evaluations([Evaluation|OtherEvaluations],
                             [VariablePair|OtherVariablePairs], OldVariables,
                             OldCalculations, SequenceVariables, SequenceCalculations,
                             [Inclusion|OtherMask]
                            ) :-
          analyze_sequence_evaluation(Evaluation, VariablePair,
                                      OldCalculations,
                                      SequenceVariables-InterimVariables,
                                      SequenceCalculations-InterimCalculations, Inclusion
                                     ),
          analyze_sequence_evaluations(OtherEvaluations, OtherVariablePairs,
                                       OldVariables, OldCalculations,
                                       InterimVariables, InterimCalculations, OtherMask).


/* this procedure finds the constant difference in the sorted list, and invokes the differences analysis.
*/

analyze_sequence_evaluation(sequence([FirstPosition-FirstValue|OtherPairs]),
                            Independent-Dependent, OldCalculations,
                            NewVariables-InterimNewVariables,
                            NewCalculations-InterimNewCalculations, Inclusion
                           ) :-
          \+ nominal_data::nominal_data(FirstPosition),
          \+ nominal_data::nominal_data(FirstValue),
          split_value_list([FirstPosition-FirstValue|OtherPairs],
                           IndependentDifference, DependentDifferences),
          difference_analysis(DependentDifferences, 0, ConstantDifference, Depth)
            -> sequence_formula(Independent, FirstPosition, Dependent,
                                IndependentDifference, ConstantDifference,
                                Depth, Formula, ApplicableFormula),
               simplify::simplify(Formula, SimplifiedFormula),
               (choose::choose(OldCalculations, SimplifiedFormula)
                  -> _ = ApplicableFormula,
                     NewVariables = InterimNewVariables,
                     NewCalculations = InterimNewCalculations,
                     Inclusion = exclude
                ; _ = OldCalculations,
                 NewVariables = [sequence(Formula)|InterimNewVariables],
                 NewCalculations = [SimplifiedFormula|InterimNewCalculations],
                 Inclusion = ApplicableFormula
               )
          ; _ = FirstPosition,
            _ = FirstValue,
            _ = OtherPairs,
            _ = Independent,
            _ = Dependent,
            _ = OldCalculations,
            NewVariables = InterimNewVariables,
            NewCalculations = InterimNewCalculations,
            Inclusion = exclude.


difference_analysis(DependentDifferences, DepthIn, ConstantDifference, DepthOut) :-
          constant_differences(DependentDifferences, ConstantDifference)
            -> DepthIn > 0,
               DepthIn = DepthOut
          ;
           difference_analysis(DependentDifferences, NextDependentDifferences),
           NextDepth is DepthIn + 1,
           difference_analysis(NextDependentDifferences, NextDepth,
                               ConstantDifference, DepthOut).


difference_analysis([_], []).

difference_analysis([Value1, Value2|OtherPairs], [Difference|OtherDifferences]) :-
          Value2 > Value1,
          Difference is Value2 - Value1,
          difference_analysis([Value2|OtherPairs], OtherDifferences).


constant_differences(DependentDifferences, ConstantDifference) :-
          constant_differences(DependentDifferences, 0, 0, 0, ConstantDifference).


constant_differences([], Count, Squares, Sum, Mean) :-
          abs(Sum) > 0,
          Mean is Sum / Count,
          StandardDeviation is sqrt((Squares - 2*Mean*Sum + Count*Mean*Mean)/ Count),
          Measure is StandardDeviation / abs(Mean),
          Measure < 0.01.

constant_differences([Difference|OtherDifferences], Count,
                     Squares, Sum, ConstantDifference) :-
          NextSquares is Squares + Difference*Difference,
          NextSum is Sum + Difference,
          NextCount is Count + 1,
          constant_differences(OtherDifferences, NextCount, NextSquares,
                               NextSum, ConstantDifference).


sequence_formula(Independent, FirstPosition, Dependent,
                 IndependentDifference, ConstantDifference,
                 Depth, Dependent - Constant * (AdjustedIndependent ** Depth),
                 formula(Value - Constant * (AdjustedPosition ** Depth), Position, Value)
                ) :-
          factorial(Depth, DepthFactorial),
          Constant is ConstantDifference / (IndependentDifference * DepthFactorial),
          (FirstPosition = 1
            -> AdjustedIndependent = Independent,
               AdjustedPosition = Position
          ;
           Adjustment is FirstPosition - 1,
           AdjustedIndependent = Independent - Adjustment,
           AdjustedPosition = Position - Adjustment
          ).


factorial(1, 1) :- !.

factorial(2, 2) :- !.

factorial(N, F) :-
          N > 2,
          K is N - 1,
          factorial(K, F1),
          F is N * F1.


split_value_list([_-Y], _, [Y]) :-
          !.

split_value_list([X1-Y1, X2-Y2|Pairs], D, [Y1|Tail]) :-
          D is X2 - X1,
          split_value_list([X2-Y2|Pairs], D, Tail).


reevaluate_sequences([], [], _,  [], [], [], [], []).

reevaluate_sequences([EvaluationIn|OtherEvaluationsIn],
                     [VariablePairIn|OtherVariablePairsIn], SequenceVariables,
                     [InclusionLocal|OtherLocalMask], [InclusionOld|OtherOldMask],
                     [EvaluationOut|OtherEvaluationsOut],
                     [VariablePairOut|OtherVariablePairsOut],
                     [InclusionOut|OtherMaskOut]
                    ) :-
          reevaluate_sequence(EvaluationIn, VariablePairIn, SequenceVariables,
                              InclusionLocal, InclusionOld, SequenceVariablesNext,
                              EvaluationOut, VariablePairOut, InclusionOut),
          reevaluate_sequences(OtherEvaluationsIn, OtherVariablePairsIn,
                               SequenceVariablesNext, OtherLocalMask, OtherOldMask,
                               OtherEvaluationsOut, OtherVariablePairsOut, OtherMaskOut).


reevaluate_sequence(sequence(Pairs), Independent-Dependent, SequenceVariablesIn,
                    LocalInclusion, OldInclusion, SequenceVariablesOut,
                    EvaluationOut, VariablePairOut, InclusionOut) :-
          LocalInclusion = exclude
            -> _ = OldInclusion,
               EvaluationOut = sequence(Pairs),
               VariablePairOut = Independent-Dependent,
               SequenceVariablesIn = SequenceVariablesOut,
               InclusionOut = exclude
          ;
           _ = Dependent,
           reevaluate_sequence_pairs(Pairs, LocalInclusion, NewPairs),
           EvaluationOut = sequence(NewPairs),
           combine_masks(LocalInclusion, OldInclusion, InclusionOut),
           SequenceVariablesIn = [SequenceVariableIn|SequenceVariablesOut],
           VariablePairOut = Independent-SequenceVariableIn.


reevaluate_sequence_pairs([], _, []).

reevaluate_sequence_pairs([Position-Image|OtherPairs], Inclusion,
                          [Position-NewValue|OtherNewPairs]) :-
          evaluate_sequence_inclusion(Position-Image, Inclusion, NewValue),
          reevaluate_sequence_pairs(OtherPairs, Inclusion, OtherNewPairs).


test_combine_masks(X) :-
          combine_masks(formula(A-3*(B ** 1), B, A), _, X).


combine_masks(formula(X, AX, BX), formula(Y, AY, BY), formula(X, AY, BY)) :-
          var(Y)
            -> AY = AX,
               BY = BX
          ;
           AX = AY,
           BX = Y.


sequence_extend_values([], _,_,_, []).

sequence_extend_values([ExpandedExperiment|OtherExpandedExperiments],
                       SequenceVariables, SequenceCalculations, Mask,
                       [NewExperiment|OtherNewExperiments]) :-
          sequence_extend_experiment(ExpandedExperiment, Mask, NewExperiment),
          sequence_extend_values(OtherExpandedExperiments, SequenceVariables,
                                 SequenceCalculations, Mask, OtherNewExperiments).


sequence_extend_experiment([], [], []).

sequence_extend_experiment([Pair|OtherValuePairs], [Inclusion|OtherMask], Experiment) :-
          (Inclusion = exclude
             -> _ = Pair,
                Experiment = OtherExperiment
           ;
            evaluate_sequence_inclusion(Pair, Inclusion, NewValue),
            Experiment = [NewValue|OtherExperiment]
          ),
          sequence_extend_experiment(OtherValuePairs, OtherMask, OtherExperiment).


evaluate_sequence_inclusion(Position-Image, Inclusion, NewValue) :-
          copy_term(Inclusion, formula(Formula,Position,Image)),
          NewValue is Formula.

:- end_object.
