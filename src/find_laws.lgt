:- object(find_laws).

:- public([find_laws/6, test_find_laws/1, test_find_laws/5, test_find_laws/6]).

/*-------------------------
 * TEST_FIND_LAWS(+SetName)
 *-------------------------
 *
 * test_find_laws/1
-----*/

test_find_laws(SetName) :-
          test_data_set::test_data_set(SetName, ConstantTolerance, LinearTolerance, ProportionalTolerance, DataTree, Variables),
          MaximumSearchVariables = 10,
          display_test_find_laws_message('DEFAULT', SetName, MaximumSearchVariables, ConstantTolerance,
                                         LinearTolerance, ProportionalTolerance, ConstantTolerance,
                                         LinearTolerance, ProportionalTolerance),
          test_find_laws(DataTree, Variables, MaximumSearchVariables, ConstantTolerance,
                         LinearTolerance, ProportionalTolerance).


/*----------------------------------------------------------------------------------------------------------------
 * TEST_FIND_LAWS(+SetName, +MaximumSearchVariables, +ConstantTolerance, +LinearTolerance, +ProportionalTolerance)
 *----------------------------------------------------------------------------------------------------------------
 *
 * test_find_laws/5 invokes find_laws/5 on the SetName test data set and displays the resulting "laws" and the time it took to run find_laws/5.
-----*/

test_find_laws(SetName, MaximumSearchVariables, ConstantTolerance, LinearTolerance, ProportionalTolerance) :-
          test_data_set::test_data_set(SetName, ConstantToleranceDEFAULT, LinearToleranceDEFAULT, ProportionalToleranceDEFAULT, DataTree, Variables),
          display_test_find_laws_message('SPECIFIED', SetName, MaximumSearchVariables, ConstantTolerance, LinearTolerance, ProportionalTolerance, ConstantToleranceDEFAULT, LinearToleranceDEFAULT, ProportionalToleranceDEFAULT),
          test_find_laws(DataTree, Variables, MaximumSearchVariables, ConstantTolerance, LinearTolerance, ProportionalTolerance).


/*-----------------------------------------------------------------------------------
 * TEST_FIND_LAWS(+DataTree, +Variables, +MaximumSearchVariables, +ConstantTolerance,
 *                +LinearTolerance, +ProportionalTolerance)
 *-----------------------------------------------------------------------------------
 *
 * test_find_laws/6
-----*/

test_find_laws(DataTree, Variables, MaximumSearchVariables, ConstantTolerance,
               LinearTolerance, ProportionalTolerance) :-
          cpu_time::cpu_time(
          	find_laws(DataTree, Variables, MaximumSearchVariables, 
                    regularities_tolerances(ConstantTolerance,
                                            linear_tolerances(LinearTolerance, ProportionalTolerance)),
                    X, Y),
          	Duration),
          display_laws::display_laws(X, Y),
          display_time(Duration).


/*---------------------
 * DISPLAY_TIME(+S, +E)
 *---------------------
 *
 * display_time/2 displays the difference of E minus S ticks as the number of seconds "to find law".
-----*/

display_time(S, E) :-
          Time is (E - S) / 60,
          compat::writeseqnl(['Time to find law:',Time,'seconds.']).

display_time(Duration) :-
          compat::writeseqnl(['Time to find law:', Duration,'seconds.']).

/*-------------------------------------------------------------
 * FIND_LAWS(+Trees, +VariablesIN, +RegularitiesTolerances, -VariablesOUT, -ReducedData)
 *-------------------------------------------------------------
 *
 * find_laws/5 analyzes the data in Trees, as described by VariablesIN,
 * putting the result of the analysis in VariablesOUT and ReducedData.

The Tree in the Trees list has the form 'ParentValue-ChildValues',
a path through the tree (from parent to child) produces the data for
a single "experiment". The simple hierarchical structure of the data
Trees can provide significant compression of the data representation of a set of experiments.

find_laws/5 analyses Trees by recursing on the ChildValues (and ChildVariables)
(the first clause), or by invoking determine_regularities/4.
Broadly speaking, find_laws/5 reduces Trees of experimental data to a single relationship among
the VariablesIN. It performs this reduction "bottom up" by reducing the bottom
subtree of each tree of Trees to a single value (applying the same reduction to
comparable subtrees in different trees), then reprocessing the "reduced" Trees
by reducing the new bottom subtrees, until all of the Trees have been reduced.

Consider the first clause. It recursively invokes find_laws/5 at two points.
The first recursion produces ChildVariablesOut and NewChildValues.
The NewChildValues can be derived values based on ChildValues.

find_laws_extend_branches/4 applies the analysis of ChildValues to OtherTrees,
repeating the calculation of any newly "invented" child variables for the child
values in OtherTrees. This process does NOT produce any new (child) variable definitions.
The result of this process is the OtherReducedTrees.

If ChildVariablesOut is of the form A-B, then there is at least one more "level"
of the tree below this one. In this case, NextVariables = [ParentVariable|ThisLevelVariables].
If there was NOT another "level" of the data Trees below this one
(i.e. the recursive invocation of find_laws/5 used the second clause, which invoked determine_regularities/4),
then NextVariables = [ParentVariable|ChildVariablesOut].

Finally, this first clause invokes find_laws/5 on the "flattened" form of
the input Tree. This recursive call will use the second clause of find_laws/5.

The second clause for find_laws/5 is invoked when the Tree (DataSet)
consists of a list of (pair) lists. Each of these contained lists
is a single experiment's data of two values, an "independent" value
and a "dependent" value. The dependent value is presumed to be some
function of the independent value. It is this function that
determine_regularities/5 is intended to find. This analysis may create
new variables by combining values of old ones, this is reflected in NewValues and VariablesOut.

The third clause for find_laws/5 is the base case, where
the Tree (DataSet) is neither a tree of values nor a list of (pair) lists.
In this case Trees = ReducedData and VariablesIN = VariablesOUT.
-----*/

find_laws([ParentValue-ChildValues|OtherTrees],
          [ParentVariable|ChildVariables],
          MaximumSearchVariables, RegularitiesTolerances,
          ParentVariablesOut-ChildVariablesOut, ReducedData
         ) :-
          !,
          find_laws(ChildValues, ChildVariables, MaximumSearchVariables, RegularitiesTolerances,
                    ChildVariablesOut, NewChildValues),
          find_laws_extend_branches(OtherTrees, ChildVariables,
                                    ChildVariablesOut, OtherReducedTrees),
          (ChildVariablesOut = ThisLevelVariables-_ThatLevelVariables
             -> NextVariables = [ParentVariable|ThisLevelVariables]
          ;
          NextVariables = [ParentVariable|ChildVariablesOut]
          ),
          find_laws([[ParentValue|NewChildValues]|OtherReducedTrees],
                    NextVariables, MaximumSearchVariables, RegularitiesTolerances,
                    ParentVariablesOut, ReducedData
                   ).

find_laws(DataSet, VariablesIn, MaximumSearchVariables, RegularitiesTolerances, VariablesOut, NewValues) :-
          DataSet \= [_-_|_],
          DataSet = [[_|_]|_],
          !,
          determine_regularities::determine_regularities(DataSet, VariablesIn,
                                 MaximumSearchVariables, RegularitiesTolerances, NewValues, VariablesOut).

find_laws(DataSet, Variables, _, _, Variables, DataSet).


/*------------------------------------------------------------------------------
 * FIND_LAWS_EXTEND_BRANCHES(+Trees, +VariablesIN, -VariablesOUT, -ReducedTrees)
 *------------------------------------------------------------------------------
 *
 * find_laws_extend_branches/4
-----*/

find_laws_extend_branches([], _, _, []).

find_laws_extend_branches([Parent-Tree|OtherTrees], VariablesIn, VariablesOut,
                          [[Parent|ReducedDataSet]|OtherReducedTrees]) :-
          find_laws_extend_branches1(Tree, VariablesIn, VariablesOut, ReducedDataSet),
          find_laws_extend_branches(OtherTrees, VariablesIn, VariablesOut, OtherReducedTrees).


/*-----------------------------------------------------------------------------------
 * FIND_LAWS_EXTEND_BRANCHES1(+DataSet, +VariablesIN, -VariablesOUT, -ReducedDataSet)
 *-----------------------------------------------------------------------------------
 *
 * find_laws_extend_branches1/4
-----*/

find_laws_extend_branches1([Parent-Tree|OtherTrees], [ParentVariable|VariablesIn],
                           ParentVariablesOut-ChildVariablesOut, ReducedExperiment) :-
          !,
          find_laws_extend_branches1(Tree, VariablesIn,
                                     ChildVariablesOut, ChildReducedExperiment),
          find_laws_extend_branches(OtherTrees, VariablesIn,
                                    ChildVariablesOut, OtherReducedExperiments),
          (ChildVariablesOut = NextChildVariablesOut-_
             -> ReducedVariables = [ParentVariable|NextChildVariablesOut]
          ; ReducedVariables = [ParentVariable|ChildVariablesOut]
          ),
          find_laws_extend_leaf([[Parent|ChildReducedExperiment]|OtherReducedExperiments],
                                ReducedVariables, ParentVariablesOut, ReducedExperiment).

find_laws_extend_branches1(DataSet, VariablesIn, VariablesOut, ReducedDataSet) :-
          DataSet \= [_-_|_],
          find_laws_extend_leaf(DataSet, VariablesIn, VariablesOut, ReducedDataSet).


/*------------------------------------------------------------------------------------
 * FIND_LAWS_EXTEND_LEAF(+Experiments, +OldVariables, -Variables, -ReducedExperiments)
 *------------------------------------------------------------------------------------
 *
 * find_laws_extend_leaf/4
-----*/

find_laws_extend_leaf(Experiments, OldVariables, Variables, ReducedExperiments) :-
          reduce_experiments::reduce_experiments(Variables, Experiments, OldVariables, ReducedExperiments).


display_test_find_laws_message(ParametersSource, SetName, MaximumSearchVariables,
                               ConstantTolerance, LinearTolerance, ProportionalTolerance,
                               ConstantToleranceDEFAULT, LinearToleranceDEFAULT, ProportionalToleranceDEFAULT) :-
          nl,
          compat::writeseqnl(['Test:', SetName]),
          compat::writeseqnl(['    ', ParametersSource, 'PARAMETERS:']),
          compat::writeseqnl(['     Max. number of search variables =', MaximumSearchVariables]),
          display_test_parameter_value('Constant tolerance', ConstantTolerance, ConstantToleranceDEFAULT),
          display_test_parameter_value('Linear tolerance', LinearTolerance, LinearToleranceDEFAULT),
          display_test_parameter_value('Proportional tolerance', ProportionalTolerance, ProportionalToleranceDEFAULT),
          nl.


display_test_parameter_value(Label, ValueIN, ValueDEFAULT) :-
          ValueIN = ValueDEFAULT
            -> compat::writeseqnl(['    ', Label, '=', ValueIN, '(recommended)'])
          ;
          compat::writeseqnl(['    ', Label, '=', ValueIN]).

:- end_object.
