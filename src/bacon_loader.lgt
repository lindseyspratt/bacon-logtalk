
:- object(bacon_loader, imports(key_value)).

:- public([
    bacon_test_id/1, run_test/1, set_test_parameters/1, set_test_parameters/4,
    set_and_run/1
    ]).

bacon_test_id(TestID) :-
          test_data_set::test_data_set(TestID, _, _, _, _, _).


run_test(TestID) :-
          ::recall(constant_tolerance, ConstantTolerance),
          ::recall(linear_tolerance, LinearTolerance),
          ::recall(proportional_tolerance, ProportionalTolerance),
          ::recall(maximum_search_variables, MaximumSearchVariables),
          test_data_set::test_data_set(TestID, ConstantToleranceDEFAULT, LinearToleranceDEFAULT, ProportionalToleranceDEFAULT, _, _),
          adjust_for_default(ConstantTolerance, ConstantToleranceDEFAULT, ConstantToleranceUSE),
          adjust_for_default(LinearTolerance, LinearToleranceDEFAULT, LinearToleranceUSE),
          adjust_for_default(ProportionalTolerance, ProportionalToleranceDEFAULT, ProportionalToleranceUSE),
          adjust_for_default(MaximumSearchVariables, 10, MaximumSearchVariablesUSE),
          find_laws::test_find_laws(TestID, MaximumSearchVariablesUSE, ConstantToleranceUSE, LinearToleranceUSE, ProportionalToleranceUSE).


adjust_for_default(ValueIN, ValueDEFAULT, ValueOUT) :-
          ValueIN == default
            -> ValueOUT = ValueDEFAULT
          ;
          ValueOUT = ValueIN.


set_test_parameters(TestID) :-
          (test_data_set::test_data_set(TestID, ConstantTolerance, LinearTolerance, ProportionalTolerance, _, _)
            -> MaximumSearchVariables = 10
          ;
          TestID == current
            -> ::recall(constant_tolerance, ConstantTolerance),
               ::recall(linear_tolerance, LinearTolerance),
               ::recall(proportional_tolerance, ProportionalTolerance),
               ::recall(maximum_search_variables, MaximumSearchVariables)
          ;
          TestID == default
            -> ConstantTolerance = 0.001,
               LinearTolerance = 0.001,
               ProportionalTolerance = 0.01,
               MaximumSearchVariables = 5
          ;
          throw(unknown_test(TestID))
          ),
          set_test_parameters(ConstantTolerance, LinearTolerance, ProportionalTolerance, MaximumSearchVariables).

set_test_parameters(ConstantTolerance, LinearTolerance, ProportionalTolerance, MaximumSearchVariables) :-
          ::remember(constant_tolerance, ConstantTolerance),
          ::remember(linear_tolerance, LinearTolerance),
          ::remember(proportional_tolerance, ProportionalTolerance),
          ::remember(maximum_search_variables, MaximumSearchVariables).

set_and_run(TestID) :-
    set_test_parameters(TestID),
    run_test(TestID).


:- end_object.
