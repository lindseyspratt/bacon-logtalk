:- category(key_value).

    :- public([recall/2, remember/2]).

    :- private([kv/2]).
    :- dynamic([kv/2]).

    recall(Key, Value) :-
        kv(Key, Value).

    remember(Key, Value) :-
        retractall(kv(Key, _)),
        assertz(kv(Key, Value)).
:- end_category.
