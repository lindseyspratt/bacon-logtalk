:- object(choose).

:- public([choose/4, choose/3, choose/2, choose_once/4, choose_once_/3, choose_once/2,
    choose_trim/3, choose_identical/3, choose_identical/2, choose_split/3]).

choose([H|T], H, [], T).

choose([H|L1], X, [H|L2], L3) :-
  choose(L1, X, L2, L3).


choose([H|T], H, T).

choose([H|L1], X, [H|L2]) :-
  choose(L1, X, L2).


choose([H|_], H).

choose([_|L1], X) :-
  choose(L1, X).


choose_once([H|T], H, [], T) :- !.

choose_once([H|L1], X, [H|L2], L3) :-
  choose_once(L1, X, L2, L3).


choose_once([H|T], H, T) :- !.

choose_once([H|L1], X, [H|L2]) :-
  choose_once(L1, X, L2).


choose_once([H|_], H) :- !.

choose_once([_|L1], X) :-
  choose_once(L1, X).


choose_trim([H|T], H, T).

choose_trim([_|L1], X, L2) :-
  choose_trim(L1, X, L2).


choose_identical([H|T], X, T) :- H == X.

choose_identical([H|L1], X, [H|L2]) :-
  choose_identical(L1, X, L2).


choose_identical([H|_], X) :- H == X.

choose_identical([_|L1], X) :-
  choose_identical(L1, X).


choose_split([H|T], H, [], T).

choose_split([H|L1], X, [H|L2], L3) :-
  choose_split(L1, X, L2, L3).


:- end_object.

