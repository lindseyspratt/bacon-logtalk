:- object(simplify).
:- public([simplify/2, test_simplify/2, new_calculation/4, uninteresting_calculation/3]).

new_calculation(monotonic(Type,XName,YName), OldVariables, OldCalculations, NewCalc) :-
          variables::variable_calculation(XName, OldVariables, OldCalculations, XCalc),
          variables::variable_calculation(YName, OldVariables, OldCalculations, YCalc),
          !,
          (Type = increasing
            -> simplify(XCalc/YCalc, NewCalc)
          ;
          Type = decreasing
            -> simplify(XCalc*YCalc, NewCalc)
          ;
          throw(invalid_type(Type))
          ).

test_simplify(1, X) :-
          simplify(a*b/a, X).

test_simplify(2, X) :-
          simplify(x/(x*(x/y)), X).

test_simplify(3, X) :-
          simplify((x/y)*(x*(x/y)), X).

test_simplify(4, X) :-
          simplify((z * x * a) / (y * x), X).


simplify(X, Y) :-
          simplify_lemma(X, Y)
          ;
          (simplify0(X, Y),
           assert_simplify_lemma(X, Y)
          ),
          !.

:- dynamic(simplify_lemma / 2).

assert_simplify_lemma(X, Y) :-
          assertz(simplify_lemma(X, Y)).


simplify0(X, Y) :-
          X =.. [Op,L,R]
            -> simplify(L, Ls),
               simplify(R, Rs),
               simplify1(Op, Ls, Rs, Y)
          ;
          X =.. [Op,L]
            -> simplify(L, Ls),
               simplify1(Op, Ls, Y)
          ;
          X = monotonic(decreasing, A, B)
            -> simplify(A * B, Y)
          ;
          X = monotonic(increasing, A, B)
            -> simplify(A / B, Y)
          ;
          X = Y.


simplify1(*, X, Y/X, Y) :- !.

simplify1(*, Y/X, X, Y) :- !.

simplify1(*, X/Y, Y/X, 1) :- !.

simplify1(*, X/Y, Z, A) :- !, simplify((X*Z) / Y, A).

simplify1(*, Z, X/Y, A) :- !, simplify((Z*X) / Y, A).

simplify1(*, A, B+C, Y) :- !, simplify(A*B+A*C, Y).

simplify1(*, B+C, A, Y) :- !, simplify(B*A+C*A, Y).

simplify1(*, A, B-C, Y) :- !, simplify(A*B-A*C, Y).

simplify1(*, B-C, A, Y) :- !, simplify(B*A-C*A, Y).

simplify1(*, X, Y*Z, A) :- !, simplify((X*Y)*Z, A).

simplify1(*, X*Y, Z, A) :- Z \= _*_, Y \= _*_, Z @< Y, !, simplify((X*Z)*Y, A).

simplify1(*, X, Y, Y*X) :- X \= _*_, Y \= _*_, Y @< X, !.

simplify1(*, 1, X, X) :- !.

simplify1(*, X, 1, X) :- !.

simplify1(/, X*Y, X, Y) :- !.

simplify1(/, X*Y, Y, X) :- !.

simplify1(/, X, Y / Z, A) :- !, simplify((X * Z) / Y, A).

simplify1(/, X/Y, Z, A) :- !, simplify(X / (Y*Z), A).

simplify1(/, X, X, 1) :- !.

simplify1(/, X, 1, X) :- !.

simplify1(/, A+B, C, Y) :- !, simplify(A/C+B/C, Y).

simplify1(/, A-B, C, Y) :- !, simplify(A/C-B/C, Y).

simplify1(/, X, Y, B) :-
  remove_multiplicand(X, A, U),
  remove_multiplicand(Y, A, V),
  !,
  simplify(U / V, B).

simplify1(+, 0, X, X) :- !.

simplify1(+, X, 0, X) :- !.

simplify1(+, A-B, C, Y) :- !, simplify((A+C)-B, Y).

simplify1(+, A, B-C, Y) :- !, simplify((A+B)-C, Y).

simplify1(+, X, Y+Z, A) :- !, simplify((X+Y)+Z, A).

simplify1(+, X+Y, Z, A) :- Z \= _+_, Y \= _+_, Z @< Y, !, simplify((X+Z)+Y, A).

simplify1(+, X, Y, Y+X) :- X \= _+_, Y \= _+_, Y @< X, !.

simplify1(-, X, X, 0) :- !.

simplify1(-, A, B-C, Y) :- !, simplify((A+C)-B, Y).

simplify1(-, A-B, C, Y) :- !, simplify(A- (B+C), Y).

simplify1(**, Base, Exponent, Y) :- !, repeat_multiplication(Base, Exponent, Y).

simplify1(Op, L, R, X) :- X =.. [Op, L, R].


simplify1(constant, L, L) :- !.

simplify1(Op, L, X) :- X =.. [Op, L].


remove_multiplicand(H, H, 1) :-
          H \= _ * _,
          !.

remove_multiplicand(T * H, H, T) :- !.

remove_multiplicand(T * H, U, V * H) :-
          !,
          remove_multiplicand(T, U, V).


repeat_multiplication(Base, 1, Base) :- !.

repeat_multiplication(Base, Exponent, NextProduct*Base) :-
          integer(Exponent),
          Exponent > 1,
          NextExponent is Exponent - 1,
          repeat_multiplication(Base, NextExponent, NextProduct).


uninteresting_calculation(OldCalc, NewCalc, OldCalcs) :-
          NewCalc = 1 / X
            -> true
          ;
          choose::choose(OldCalcs, NewCalc)
            -> true
          ;
          NewCalc = X / Y
            -> choose::choose(OldCalcs, Y / X)
          ;
          primitive_elements(OldCalc, OldPrimitives),
          primitive_elements(NewCalc, NewPrimitives),
          OldPrimitives \= NewPrimitives.
          

primitive_elements(Calc, SortedElements) :-
          primitive_elements_dl(Calc, Elements-[]),
          sort(Elements, SortedElements),
          !.


primitive_elements_dl(Calc, Elements-Tail) :-
          Calc = monotonic(_,X,Y)
            -> primitive_elements_list([X,Y], Elements-Tail)
          ;
          (Calc = slope(X,Y);Calc = intercept(X, Y))
            -> Elements = [Calc|Tail]
          ;
          atom(Calc)
            -> Elements = [Calc|Tail]
          ;
          number(Calc)
            -> Elements = Tail
          ;
          Calc =.. [_|Args],
          primitive_elements_list(Args, Elements-Tail).


primitive_elements_list([], Tail-Tail).

primitive_elements_list([Arg|OtherArgs], Elements-Tail) :-
          primitive_elements_dl(Arg, Elements-InterimTail),
          primitive_elements_list(OtherArgs, InterimTail-Tail).

:- end_object.
