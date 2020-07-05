:- object(infer_constants).
:- public([infer_constants/7]).

infer_constants([TestValues|OtherTestValues], DependentVariables,  Tolerance, ConstantDependentVariables,
                StrictConstantVariables, ConstantValues-ValueTail,
                NewVariables-NewVariablesTail) :-
          check_constants(OtherTestValues, TestValues, ConstantEvaluations),
          analyze_constant_evaluations(ConstantEvaluations, DependentVariables, Tolerance,
                                       ConstantDependentVariables, StrictConstantVariables,
                                       ConstantValues-ValueTail,
                                       NewVariables-NewVariablesTail).


analyze_constant_evaluations([], [], _, [], [], T1-T1, T2-T2).

analyze_constant_evaluations([nominal|OtherConstantEvaluations], [_|OtherDependentVariables], Tolerance,
                             ConstantDependentVariables, StrictConstantVariables,
                             ConstantValues-ValuesTail, ConstantNames-VariablesTail) :-
          analyze_constant_evaluations(OtherConstantEvaluations, OtherDependentVariables, Tolerance,
                                       ConstantDependentVariables,
                                       StrictConstantVariables,
                                       ConstantValues-ValuesTail, ConstantNames-VariablesTail).


analyze_constant_evaluations([constant(Count,SumOfSquares,SimpleSum) | OtherConstantEvaluations],
                             [DependentVariable | OtherDependentVariables],
                             Tolerance,
                             ConstantDependentVariables,
                             StrictConstantVariables,
                             ConstantValues-ValuesTail,
                             ConstantNames-VariablesTail
                            ) :-
          constant_approximation(Count, SumOfSquares, SimpleSum, Mean, StandardDeviation),
          (Mean =:= 0
            -> Measure = StandardDeviation
          ;
           Measure is StandardDeviation / abs(Mean)
          ),
          (Measure =<  Tolerance % 0.001 % Imprecision limit
            -> [Mean|ConstantValuesNext] = ConstantValues,
               [constant(DependentVariable)|ConstantNamesNext] = ConstantNames,
               [DependentVariable|ConstantDependentVariablesNext] = ConstantDependentVariables,
               (Measure =< 0.00001
                 -> [DependentVariable|StrictConstantVariablesNext] = StrictConstantVariables
               ;
                StrictConstantVariablesNext = StrictConstantVariables
               )
          ;
           ConstantValuesNext = ConstantValues,
           ConstantNamesNext = ConstantNames,
           ConstantDependentVariablesNext = ConstantDependentVariables,
           StrictConstantVariablesNext = StrictConstantVariables
          ),
          analyze_constant_evaluations(OtherConstantEvaluations, OtherDependentVariables,
                                       Tolerance,
                                       ConstantDependentVariablesNext,
                                       StrictConstantVariablesNext,
                                       ConstantValuesNext-ValuesTail,
                                       ConstantNamesNext-VariablesTail
                                      ).


check_constants([], Values, Values).

check_constants([TestValues|OtherTests], ValuesIn, ValuesOut) :-
          check_test_constants(TestValues, ValuesIn, ValuesNext),
          check_constants(OtherTests, ValuesNext, ValuesOut).


check_test_constants([], [], []).

check_test_constants([TestValue|OtherTestValues],
                     [CheckValueIn|OtherCheckValuesIn],
                     [CheckValueOut|OtherCheckValuesOut]) :-
          (nominal_data::nominal_data(TestValue)
            -> CheckValueOut = nominal
          ;
          CheckValueIn = constant(Count,SumOfSquares,SimpleSum)
            -> NewCount is Count + 1,
               NewSumOfSquares is SumOfSquares + TestValue * TestValue,
               NewSimpleSum is SimpleSum + TestValue,
               CheckValueOut = constant(NewCount, NewSumOfSquares, NewSimpleSum)
          ;
           Square is CheckValueIn * CheckValueIn + TestValue * TestValue,
           Sum is TestValue + CheckValueIn,
           CheckValueOut = constant(2, Square, Sum)
          ),
          check_test_constants(OtherTestValues, OtherCheckValuesIn, OtherCheckValuesOut).


constant_approximation(Count, SumOfSquares, SimpleSum, Mean, StandardDeviation) :-
          Mean is SimpleSum / Count,
          MeanSquare is Mean*Mean,
          StandardDeviation is sqrt((SumOfSquares  - 2*Mean*SimpleSum + Count*MeanSquare)
                                    / Count).

:- end_object.
