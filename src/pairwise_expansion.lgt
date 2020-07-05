:- object(pairwise_expansion).

:- public([pairwise_expansion/2,pairwise_expansion/3]).

pairwise_expansion([], []).
pairwise_expansion([Hin|Tin], Pairs) :-
  pairwise_expansion(Tin, Hin, Pairs-Tail),
  pairwise_expansion(Tin, Tail).

pairwise_expansion([], _, Tail-Tail).
pairwise_expansion([Hin|Tin], X, [X-Hin|Pairs]-Tail) :-
  pairwise_expansion(Tin, X, Pairs-Tail).

:- end_object.
