:- object(intersect).
:- public([intersect/3]).

difference(Minuend, Subtrahend, Difference) :-
  intersect_difference(Minuend, Subtrahend, _, Difference, _).


intersect(A, B, Intersection) :-
  intersect_difference(A, B, Intersection, _, _).


intersect_difference(A, B, C, AD, BD) :-
  (A = []; B = [])
    -> C = [], AD = A, BD = B
  ; A = [HA|TA],
    (choose::choose(B, HA, OtherB)
      -> C = [HA|TC],
           intersect_difference(TA, OtherB, TC, AD, BD)
      ; AD = [HA|ADT],
        intersect_difference(TA, B, C, ADT, BD)
    ).


/* intersect_ordered(+L1, +L2, ?Intersect)
L1 and L2 must be instantiated.
This is a little bit faster than using intersect_difference_ordered/5 and just ignoring the difference arguments (about 1 millisecond on lists of length 11 and 5, with a non-empty intersection on a MacII with LPA MacProlog 2.8).
*/

intersect_ordered([], _, []) .

intersect_ordered([H1|T1], L2, Intersect) :-
  intersect_ordered_2(L2, H1, T1, Intersect).


intersect_ordered_2([], _H1, _T1, []).

intersect_ordered_2([H2|T2], H1, T1, Intersect) :-
  compare(Order, H1, H2),
  intersect_ordered_3(Order, H1, T1, H2, T2, Intersect).


intersect_ordered_3(<, _H1, T1, H2, T2, Intersect) :-
  intersect_ordered_2(T1, H2, T2, Intersect).

intersect_ordered_3(=, H1, T1, _, T2, [H1|Intersect]) :-
  intersect_ordered(T1, T2, Intersect).

intersect_ordered_3(>, H1, T1, _H2, T2, Intersect) :-
  intersect_ordered_2(T2, H1, T1, Intersect).


/* intersect_difference_ordered(+L1, +L2, ?Intersect, ?L1Diff, ?L2Diff)
L1 and L2 must be instantiated.

[This is a re-implementation.  The previous version used @< and == directly, and a large if-then-else construct.  This reimplementation is about 10 times faster than the old version.  It is modeled on code by R. A. O'Keefe in "The Craft of Prolog", p. 57 for ord_union/3.]
*/

intersect_difference_ordered([], L2, [], [], L2) .

intersect_difference_ordered([H1|T1], L2, Intersect, L1Diff, L2Diff) :-
  intersect_difference_ordered_2(L2, H1, T1, Intersect, L1Diff, L2Diff).


intersect_difference_ordered_2([], H1, T1, [], [H1|T1], []).

intersect_difference_ordered_2([H2|T2], H1, T1, Intersect, L1Diff, L2Diff) :-
  compare(Order, H1, H2),
  intersect_difference_ordered_3(Order, H1, T1, H2, T2, Intersect, L1Diff, L2Diff).


intersect_difference_ordered_3(<, H1, T1, H2, T2, Intersect, [H1|OtherL1Diff], L2Diff) :-
  intersect_difference_ordered_2(T1, H2, T2, Intersect, L2Diff, OtherL1Diff).

intersect_difference_ordered_3(=, H1, T1, _, T2, [H1|Intersect], L1Diff, L2Diff) :-
  intersect_difference_ordered(T1, T2, Intersect, L1Diff, L2Diff).

intersect_difference_ordered_3(>, H1, T1, H2, T2, Intersect, L1Diff, [H2|OtherL2Diff]) :-
  intersect_difference_ordered_2(T2, H1, T1, Intersect, L1Diff, OtherL2Diff).


difference_ordered(L1, L2, D) :-
          intersect_difference_ordered(L1, L2, _, D, _).

:- end_object.
