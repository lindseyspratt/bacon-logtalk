:- object(determine_regularities).
:- public([determine_regularities/6]).

test_determine_regularities(SetName, X,Y) :-
          test_data_set::test_data_set(SetName, ConstantTolerance, LinearTolerance, ProportionalTolerance,
                        DataTree, Variables),
          determine_regularities(DataTree, Variables, 10, 
                                 regularities_tolerances(ConstantTolerance,
                                                         linear_tolerances(LinearTolerance,
                                                                           ProportionalTolerance)),
                                 X,Y).


/*--------------------------------------------------------------------------------
 * DETERMINE_REGULARITIES(+TestValues, +VariablesIn, +MaximumSearchVariables,
 *                        +RegularitiesTolerances, +FinalValues, +FinalVariables)
 *--------------------------------------------------------------------------------
 *
 * determine_regularities/6
-----*/

determine_regularities(TestValues, VariablesIn,
                       MaximumSearchVariables,
                       regularities_tolerances(ConstantTolerance, LinearTolerances),
                       FinalValues, FinalVariables
                      ) :-
          determine_regularities_message,
          expand::expand_variables(VariablesIn, ExpandedVariables),
          expand::expand_test_values(TestValues, ExpandedTestValues),
          determine_regularities1(ExpandedTestValues, ExpandedVariables,
                                  MaximumSearchVariables, ConstantTolerance, LinearTolerances,
                                  FinalValues, FinalVariables).


/*------------------------------------------------------------------------------------------------
 * DETERMINE_REGULARITIES1(+PairTestValues, +VariablePairs, +MaximumSearchVariables, +ConstantTolerance, +LinearTolerances,
 *                         -FinalValues, -FinalVariables)
 *------------------------------------------------------------------------------------------------
 *
 * determine_regularities1/7
-----*/

determine_regularities1([], [], _, _, _, [], []).

determine_regularities1([PairTestValues|OtherPairTestValues],
                        [VariablePair|OtherVariablePairs],
                        MaximumSearchVariables, ConstantTolerance, LinearTolerances,
                        FinalValues, FinalVariables) :-
          determine_regularities2(PairTestValues, VariablePair,
                                  MaximumSearchVariables, ConstantTolerance, LinearTolerances,
                                  VariablePair, VariablePair, VariablePair,
                                  RegularValues, RegularVariables),
          interesting_regularities::interesting_regularities(VariablePair, RegularValues, RegularVariables,
                                   PossibleFinalValues, PossibleFinalVariables),
          (PossibleFinalVariables = []
             -> compat::append(RegularValues, OtherFinalValues, FinalValues),
                compat::append(RegularVariables, OtherFinalVariables, FinalVariables),
                _ = PossibleFinalValues
          ;
          compat::append(PossibleFinalValues, OtherFinalValues, FinalValues),
          compat::append(PossibleFinalVariables, OtherFinalVariables, FinalVariables)
          ),
          determine_regularities1(OtherPairTestValues, OtherVariablePairs,
                                  MaximumSearchVariables, ConstantTolerance, LinearTolerances,
                                  OtherFinalValues, OtherFinalVariables).


determine_regularities2(_, [], _, _, _, _, _, _, [], []).

determine_regularities2(TestValues, VariablesIn,
                        MaximumSearchVariables, ConstantTolerance, LinearTolerances,
                        DependentCalculations, TotalVariables, TotalCalculations,
                        FinalValues, FinalVariables) :-
          VariablesIn \= [],
          determine_regularities_message(VariablesIn, TestValues),
          infer_constants::infer_constants(TestValues, VariablesIn, ConstantTolerance, ConstantVariables,
                          StrictConstantVariables, FinalValues-InterimValues1,
                          FinalVariables-InterimVariables1),
          trim_test_values::trim_test_values(TestValues, VariablesIn,
                           DependentCalculations, StrictConstantVariables,
                           TrimmedTestValues1, TrimmedDependentVariables1,
                           _TrimmedCalculations1),
          infer_linear::infer_linear(TrimmedTestValues1, TrimmedDependentVariables1, LinearTolerances,
                       TotalVariables, TotalCalculations, LinearVariables,
                       InterimValues1-InterimValues2, InterimVariables1-InterimVariables2),
          compat::append(ConstantVariables, LinearVariables, ConsumedVariables),
          (VariablesIn = [IndependentVariable|_],
           choose::choose(ConsumedVariables, ConsumedVariable),
           interesting_regularities::depends_on(ConsumedVariable, IndependentVariable)
             -> InterimValues2 = [],
                InterimVariables2 = []
           ;
           % '2' adjusts for the independent variable and one dependent variable
           length(VariablesIn, VariablesInLength),
           VariablesInLength - 2 >= MaximumSearchVariables
             -> !,
                determine_regularities_failure_message(VariablesInLength, MaximumSearchVariables),
                fail
           ;
           consider_monotonic::consider_monotonic(TestValues, VariablesIn,
                               DependentCalculations, TotalVariables,
                               TotalCalculations, TestValues3,
                               Variables3, Calculations3,
                               MonotonicVariables, MonotonicCalculations),
           consider_sequence::consider_sequence(TestValues3, Variables3, Calculations3,
                              TotalVariables, TotalCalculations,
                              TestValues4, Variables4, Calculations4,
                              SequenceVariables, SequenceCalculations),
           (Variables4 = VariablesIn
               -> InterimValues2 = [],
                  InterimVariables2 = [],
                  _ = ConsumedVariables,
                  _ = MonotonicVariables,
                  _ = MonotonicCalculations,
                  _ = TestValues4,
                  _ = Calculations4,
                  _ = SequenceVariables,
                  _ = SequenceCalculations
             ;
              _ = ConsumedVariables,
              compat::append(TotalVariables, MonotonicVariables, InterimTotalVariables),
              compat::append(TotalCalculations, MonotonicCalculations,
                     InterimTotalCalculations),
              compat::append(InterimTotalVariables, SequenceVariables, NextTotalVariables),
              compat::append(InterimTotalCalculations, SequenceCalculations,
                     NextTotalCalculations),
              determine_regularities2(TestValues4, Variables4,
                                      MaximumSearchVariables,
                                      ConstantTolerance, LinearTolerances,
                                      Calculations4, NextTotalVariables,
                                      NextTotalCalculations, InterimValues2,
                                      InterimVariables2)
           )
          ).


determine_regularities_message :-
          write('Start to determine regularities.'),
          nl.

determine_regularities_message(VariablesIn, TestValues) :-
          write('Determine regularities for: '),
          compat::writenl(VariablesIn),
          compat::writenl(TestValues),
          nl.

determine_regularities_failure_message(VariablesInLength, MaximumSearchVariables) :-
          write('FAILED to determine regularities. Too many "search" variables: '),
          write(VariablesInLength),
          write(' variables (excluding independent variable and one dependent variable) but '),
          write(MaximumSearchVariables),
          write(' variables is the maximum allowed.'),
          nl.

:- end_object.
