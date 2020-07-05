:- object(compat).

:- public([append/3,writeseqnl/1,writenl/1]).

append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).

writeseqnl([]) :- nl.
writeseqnl([H|T]) :-
    write(H),
    writeseqnl1(T).

writeseqnl1([]) :- nl.
writeseqnl1([H|T]) :-
    write(' '),
    write(H),
    writeseqnl1(T).

writenl(T) :-
    write(T),
    nl.

:- end_object.
