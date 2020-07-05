:- object(trim_test_values).
:- public([trim_test_values/7,trim_test_values/3]).

trim_test_values(TestValues, Variables, Calculations, ConsumedVariables,
                 TrimmedTestValues, TrimmedVariables, TrimmedCalculations) :-
          trim_mask(Variables, ConsumedVariables, Mask),
          trim_list_by_mask(Variables, Mask, TrimmedVariables),
          trim_list_by_mask(Calculations, Mask, TrimmedCalculations),
          trim_test_values(TestValues, Mask, TrimmedTestValues).


trim_test_values([], _, []).

trim_test_values([TestValues|OtherTestValues], Mask,
                 [TrimmedTestValues|OtherTrimmedTestValues]) :-
          trim_list_by_mask(TestValues, Mask, TrimmedTestValues),
          trim_test_values(OtherTestValues, Mask, OtherTrimmedTestValues).


trim_list_by_mask([], _, []).

trim_list_by_mask([H|T], [Hm|Tm], TrimmedList) :-
          (exclude = Hm
             -> TrimmedList = TrimmedListTail
           ;
           include = Hm
             -> TrimmedList = [H|TrimmedListTail]
           ;
           throw(undefined_trim(Hm))
          ),
          trim_list_by_mask(T, Tm, TrimmedListTail).


trim_mask([], _, []).

trim_mask([H|T], Exclusions, [Hm|Tm]) :-
          (choose::choose(Exclusions, H)
             -> Hm = exclude
           ;
           Hm = include
          ),
          trim_mask(T, Exclusions, Tm).

:- end_object.

