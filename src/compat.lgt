:- object(compat).

:- public([writeseqnl/1,writenl/1]).

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
