% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(counters,
  [show_counters/1, increment/1, init_counters/0,
   init_input_counters/2, get_file_counter/2]).

:- use_module([cdcl, display]).

%  Counters for:
%    clauses, variables, units propagated,
%    decisions made, conflicts encountered,
%    generating file names for implication graphs and assignment trees

:- dynamic
     clause_counter/1, variable_counter/1, unit_counter/1, 
     decision_counter/1, conflict_counter/1,
     ig_file_counter/1, at_file_counter/1.

%  Initialization - set counters to 0
%  Initialization - number of Clauses and Variables

init_counters :-
  retractall(unit_counter(_)),      assert(unit_counter(0)),
  retractall(decision_counter(_)),  assert(decision_counter(0)),
  retractall(conflict_counter(_)),  assert(conflict_counter(0)),
  retractall(ig_file_counter(_)),   assert(ig_file_counter(0)),
  retractall(at_file_counter(_)),   assert(at_file_counter(0)).

init_input_counters(Number_of_Clauses, Number_of_Variables) :-
  retractall(clause_counter(_)),
  assert(clause_counter(Number_of_Clauses)),
  retractall(variable_counter(_)), 
  assert(variable_counter(Number_of_Variables)).

%  increment/1   - increment a counter

increment(unit) :-
  retract(unit_counter(N)),
  N1 is N + 1,
  assert(unit_counter(N1)).
increment(decision) :-
  retract(decision_counter(N)),
  N1 is N + 1,
  assert(decision_counter(N1)).
increment(conflict) :-
  retract(conflict_counter(N)),
  N1 is N + 1,
  assert(conflict_counter(N1)).
increment(ig) :-
  retract(ig_file_counter(N)),
  N1 is N + 1,
  assert(ig_file_counter(N1)).
increment(at) :-
  retract(at_file_counter(N)),
  N1 is N + 1,
  assert(at_file_counter(N1)).


%  show_counters    - write the counters
  
show_counters(Mode) :-
  write('Statistics: clauses='),
  clause_counter(Clause_Count),
  write(Clause_Count),
  write(', variables='),
  variable_counter(Variable_Count),
  write(Variable_Count),
  unit_counter(Unit_Count), 
  write(', units='),
  write(Unit_Count),
  decision_counter(Decision_Count),
  write(', decisions='),
  write(Decision_Count),
  conflict_counter(Conflict_Count),
  write(', conflicts='),
  write(Conflict_Count),
  learned_clauses_count(Mode),
  nl.

% learned_clauses_count
%   Count of learned clauses in non-dpll mode

learned_clauses_count(dpll) :- !.
learned_clauses_count(_) :-
  learned(Clauses),
  length(Clauses, L),
  write(', learned clauses='),
  write(L).


%  get_file_counter/2 - return the file counter
%    Counter - ig for implication graph, at for assignment tree
%    N - returned value

get_file_counter(ig, N) :- ig_file_counter(N).
get_file_counter(at, N) :- at_file_counter(N).
