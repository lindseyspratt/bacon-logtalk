:- object(expand).
:- public([expand_variables/2,expand_test_values/2]).

expand_variables([IndependentVariable|OtherVariables], VariablePairs) :-
  pairwise_expansion::pairwise_expansion(OtherVariables, IndependentVariable, HyphenatedVariablePairs - []),
  hyphenation_to_list(HyphenatedVariablePairs, VariablePairs).

hyphenation_to_list([], []).
hyphenation_to_list([X-Y|OtherHyphenatedVariables], [[X,Y]|OtherVariablePairs]) :-
  hyphenation_to_list(OtherHyphenatedVariables, OtherVariablePairs).

expand_test_values(DataSet, PairDataSets) :-
  expand_test_values1(DataSet, PairwiseData),
  reorganize_pairwise_data(PairwiseData, PairDataSets).

expand_test_values1([], []).
expand_test_values1([[IndependentData|OtherData]|OtherExperiments], [PairwiseData|OtherPairwiseData]) :-
  pairwise_expansion::pairwise_expansion(OtherData, IndependentData, PairwiseData-[]),
  expand_test_values1(OtherExperiments, OtherPairwiseData).

reorganize_pairwise_data([], []).
reorganize_pairwise_data([], [[]|OtherPairDataSets]) :-
  reorganize_pairwise_data([], OtherPairDataSets).
reorganize_pairwise_data([PairwiseExperiments|OtherPairwiseExperiments], PairDataSets) :-
  reorganize_pairwise_experiments(PairwiseExperiments, PairDataSets, OtherPairDataSets),
  reorganize_pairwise_data(OtherPairwiseExperiments, OtherPairDataSets).

reorganize_pairwise_experiments([], [], []).
reorganize_pairwise_experiments([IndependentValue-DependendentValue|OtherPairExperiments], [[[IndependentValue,DependendentValue]|OtherData]|OtherPairDataSets], [OtherData|OtherOtherData]) :-
  reorganize_pairwise_experiments(OtherPairExperiments, OtherPairDataSets, OtherOtherData).

:- end_object.
