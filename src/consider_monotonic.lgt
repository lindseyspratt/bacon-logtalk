:- object(consider_monotonic).

:- public([consider_monotonic/10, extend_test_values/9]).

/*----------------------------------------------------------------------------------
 * CONSIDER_MONOTONIC(+TestValuesIn, +VariablesIn, +CalculationsIn, +TotalVariables,
 *                    +TotalCalculations, -TestValuesOut, -VariablesOut,
 *                    -CalculationsOut, -MonotonicVariables, -MonotonicCalculations)
 *----------------------------------------------------------------------------------
 *
 * consider_monotonic/10
-----*/

consider_monotonic([TestValuesIn|OtherTestValuesIn], VariablesIn, CalculationsIn,
                   TotalVariables, TotalCalculations, TestValuesOut, VariablesOut,
                   CalculationsOut, MonotonicVariables, MonotonicCalculations) :-
          pairwise_expansion::pairwise_expansion(TestValuesIn, ExpandedValues),
          check_monotonic(OtherTestValuesIn, ExpandedValues, Evaluations, NewTestValues),
          pairwise_expansion::pairwise_expansion(VariablesIn, ExpandedVariables),
          analyze_monotonic_evaluations(Evaluations, ExpandedVariables, TotalVariables,
                                        TotalCalculations,  MonotonicVariables,
                                        MonotonicCalculations, Mask),
          trim_test_values::trim_test_values(NewTestValues, Mask, TrimmedNewTestValues2),
          extend_test_values([TestValuesIn|OtherTestValuesIn], VariablesIn, CalculationsIn,
                             MonotonicVariables, MonotonicCalculations,
                             TrimmedNewTestValues2, TestValuesOut, VariablesOut,
                             CalculationsOut
                            ).


check_monotonic([], Evaluations, Evaluations, []).

check_monotonic([TestValues|OtherTestValues], EvaluationsIn, EvaluationsOut, NewTestValues) :-
          pairwise_expansion::pairwise_expansion(TestValues, ExpandedTestValues),
          check_test_monotonic(ExpandedTestValues, EvaluationsIn, EvaluationsNext,
                               LocalNewTestValues),
          compat::append(LocalNewTestValues, OtherNewTestValues, NewTestValues),
          check_monotonic(OtherTestValues, EvaluationsNext, EvaluationsOut, OtherNewTestValues).


check_test_monotonic([], [], [], Out) :-
          Out = [[]] -> true
          ;
          Out = [[],[]].

check_test_monotonic([TestValue|OtherTestValues],
                     [CheckValueIn|OtherCheckValuesIn],
                     [CheckValueOut|OtherCheckValuesOut],
                     NewValues
                    ) :-
          (monotonic(TestValue, CheckValueIn, CheckValueOut, LocalNewValues)
             -> (LocalNewValues = [LocalValue1, LocalValue2]
                   -> NewValues = [[LocalValue1|OtherLocalValues1],
                                   [LocalValue2|OtherLocalValues2]],
                      OtherNewValues = [OtherLocalValues1, OtherLocalValues2]
                 ;
                 LocalNewValues = [LocalValue]
                   -> NewValues = [[LocalValue|OtherLocalValues]],
                      OtherNewValues = [OtherLocalValues]
                 ;
                 throw(invalid_local_new_values(LocalNewValues))
                )
          ;
           CheckValueOut = '*nonmonotonic*',
           (NewValues = [['*nonmonotonic*'|OtherLocalValues]]
              -> OtherNewValues = [OtherLocalValues]
            ;
            NewValues = [['*nonmonotonic*'|OtherLocalValues1],
                         ['*nonmonotonic*'|OtherLocalValues2]]
              -> OtherNewValues = [OtherLocalValues1, OtherLocalValues2]
            ;
            throw(invalid_new_values(NewValues))
           )
          ),
          check_test_monotonic(OtherTestValues, OtherCheckValuesIn,
                               OtherCheckValuesOut, OtherNewValues).


monotonic(TestX-TestY, CheckIn, CheckOut, NewValues) :-
          CheckIn = InitialX-InitialY
            -> \+ nominal_data::nominal_data(TestX),
               \+ nominal_data::nominal_data(TestY),
               AIX is abs(InitialX),
               AIY is abs(InitialY),
               ATX is abs(TestX),
               ATY is abs(TestY),
               (ATX > AIX
                  -> (ATY > AIY
                        -> IR is InitialX / InitialY,
                           TR is TestX / TestY,
                           NewValues = [IR, TR],
                           CheckOut = monotonic(increasing, [AIX-AIY, ATX-ATY])
                      ;
                      ATY < AIY
                        -> IP is InitialX * InitialY,
                           TP is TestX * TestY,
                           NewValues = [IP, TP],
                           CheckOut = monotonic(decreasing, [AIX-AIY, ATX-ATY])
                      ;
                      throw(invalid_aty_aiy1(ATY, AIY))
                      )
               ;
               ATX < AIX
                 -> (ATY < AIY
                       -> IR is InitialX / InitialY,
                          TR is TestX / TestY,
                          NewValues = [IR, TR],
                          CheckOut = monotonic(increasing, [ATX-ATY, AIX-AIY])
                     ;
                     ATY > AIY
                       -> IP is InitialX * InitialY,
                          TP is TestX * TestY,
                          NewValues = [IP, TP],
                          CheckOut = monotonic(decreasing, [ATX-ATY, AIX-AIY])
                     ;
                     throw(invalid_aty_aiy2(ATY, AIY))
                     )
               ;
               throw(invalid_atx_aix(ATX, AIX))
               )
          ;
          CheckIn = monotonic(Type, PairsList)
            -> ATX is abs(TestX),
               ATY is abs(TestY),
               insert_pair(PairsList, Type, ATX-ATY, NewPairsList),
               (Type = increasing
                  -> V is TestX / TestY
                ;
                Type = decreasing
                  -> V is TestX * TestY
                ;
                throw(invalid_type(Type))
               ),
               NewValues = [V],
               CheckOut = monotonic(Type, NewPairsList)
          ;
          throw(invalid_check(CheckIn)).


insert_pair([], _, TX-TY, [TX-TY]).

insert_pair([X-Y|OtherPairs], Type, TX-TY, NewPairsList) :-
          X > TX
            -> (Type = increasing
                  -> Y > TY,
                     NewPairsList = [TX-TY, X-Y|OtherPairs]
                ;
                Type = decreasing
                  -> Y < TY,
                     NewPairsList = [TX-TY, X-Y|OtherPairs]
                ;
                throw(invalid_insert_type1(Type))
               )
          ;
          % X =< TX
          (Type = increasing
              -> Y < TY
            ;
            Type = decreasing
              -> Y > TY
            ;
            throw(invalid_insert_type2(Type))
          ),
          NewPairsList = [X-Y|OtherNewPairs],
          insert_pair(OtherPairs, Type, TX-TY, OtherNewPairs).


trim_new_test_values([], _, []).

trim_new_test_values([NewTestValues|OtherNewTestValues], Evaluations,
                     [TrimmedNewTestValues|OtherTrimmedNewTestValues]) :-
          trim_new_test_values1(NewTestValues, Evaluations, TrimmedNewTestValues),
          trim_new_test_values(OtherNewTestValues, Evaluations, OtherTrimmedNewTestValues).


trim_new_test_values1([], [], []).

trim_new_test_values1([NewTestValue|OtherNewTestValues],
                      [Evaluation|OtherEvaluations], TrimmedNewTestValues) :-
          (Evaluation = '*nonmonotonic*'
             -> TrimmedNewTestValues = OtherTrimmedNewTestValues
           ;
           TrimmedNewTestValues = [NewTestValue|OtherTrimmedNewTestValues]
          ),
          trim_new_test_values1(OtherNewTestValues, OtherEvaluations, OtherTrimmedNewTestValues).


analyze_monotonic_evaluations([], [], _, _, [], [], []).

analyze_monotonic_evaluations([Evaluation|OtherEvaluations],
                              [XName-YName|OtherVariablePairs],
                              OldVariables, OldCalculations,
                              NewVariables, Calculations, Mask) :-
          (Evaluation = '*nonmonotonic*'
             -> NewVariablesNext = NewVariables,
                CalculationsNext = Calculations,
                Mask = [exclude|OtherMask]
           ;
           choose::choose(OldVariables, monotonic(_,XName,YName))
             -> NewVariablesNext = NewVariables,
                CalculationsNext = Calculations,
                Mask = [exclude|OtherMask]
           ;
           Evaluation = monotonic(Type, _)
             -> NewVariable = monotonic(Type,XName,YName),
                simplify::new_calculation(NewVariable, OldVariables, OldCalculations, NewCalc),
                (simplify::uninteresting_calculation(NewVariable, NewCalc, OldCalculations)
                   -> NewVariablesNext = NewVariables,
                      CalculationsNext = Calculations,
                      Mask = [exclude|OtherMask]
                 ;
                 [NewCalc|CalculationsNext] = Calculations,
                 [NewVariable|NewVariablesNext] = NewVariables,
                 Mask = [include|OtherMask]
                )
           ;
           throw(invalid_monotonic(Evaluation, OldVariables))
          ),
          analyze_monotonic_evaluations(OtherEvaluations, OtherVariablePairs,
                                        OldVariables, OldCalculations,
                                        NewVariablesNext, CalculationsNext, OtherMask).


extend_test_values(TestValues, Variables, Calculations, [], [], _,
                   TestValues, Variables, Calculations ).

extend_test_values([], VariablesIn, CalculationsIn, NewVariables,
                   NewCalculations, _, [], VariablesOut , CalculationsOut ) :-
          NewVariables \= [],
          compat::append(VariablesIn, NewVariables, VariablesOut),
          compat::append(CalculationsIn, NewCalculations, CalculationsOut).

extend_test_values([TestValuesIn|OtherTestValuesIn], VariablesIn, CalculationsIn,
                   NewVariables, NewCalculations, [NewTestValues|OtherNewTestValues],
                   [TestValuesOut|OtherTestValuesOut], VariablesOut , CalculationsOut) :-
          NewVariables \= [],
          compat::append(TestValuesIn, NewTestValues, TestValuesOut),
          extend_test_values(OtherTestValuesIn, VariablesIn, CalculationsIn,
                             NewVariables, NewCalculations, OtherNewTestValues,
                             OtherTestValuesOut, VariablesOut , CalculationsOut).
  

:- end_object.
