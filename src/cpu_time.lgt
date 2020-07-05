:- object(cpu_time).

:- public([cpu_time_sec/1, cpu_time/1, cpu_time/2, cpu_time/3]).

:- meta_predicate(cpu_time(0,*)).
:- meta_predicate(cpu_time(*,0,*)).

/* These predicates are from R.A.O'Keefes' "Craft of Prolog", p. 82.
*/

/* cpu_time_sec(-Time) unifies Time with the cpu time since the Prolog started. */

cpu_time_sec(Time) :-
	cpu_time(T),
	Time is T / 1000.0 .
%  !,
%  ticks(T), % in (1/60) second units
%  Time is T/60.0.

/* cpu_time(+Goal, -Duration) unifies Duration with the cpu time taken to call Goal once until it either succeeds or fails. This procedure should only be used when Goal takes a long time.
*/

cpu_time(Goal, Duration) :-
	!,
	cpu_time_sec(T1),
	%  ticks(T1), % in (1/60) second units
	(call(Goal) -> true; true),
	%  ticks(T2),
	cpu_time_sec(T2),
	Duration is (T2-T1).

/* cpu_time(+N, +Goal, -Duration) unifies Duration with the cpu time taken to (call Goal until it either succeeds or fails) N times.
*/

cpu_time(N, Goal, Duration) :-
  !,
  cpu_time((ct_repeat(N), (Goal -> fail;true); true), D1),
  cpu_time((ct_repeat(N), (true -> fail;true); true), D2),
  Duration is D1 - D2.

:- if(current_logtalk_flag(prolog_dialect, swi)).

    % SWI-Prolog specific code
    cpu_time(Time) :-
      statistics(cputime, Time).

:- elif(current_logtalk_flag(prolog_dialect, gnu)).

    % GNU Prolog
    cpu_time(Time) :-
      statistics(cpu_time, [Time, _]).

:- endif.

ct_repeat(N) :-
  integer(N),
  N> 0,
  ct_repeat_1(N).

ct_repeat_1(1) :- !.
ct_repeat_1(_).
ct_repeat_1(N) :- M is N-1, ct_repeat_1(M).

:- end_object.
