% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(io, [
  write_assignment/1, write_assignment/2, write_assignments/1,
  write_literal/1, write_paths/1, write_graph/1
  ]).

:- use_module([auxpred, counters, modes, config]).

:- op(610, fy,  ~).

%  IO predicates for displaying assignments,
%    and implication and dominator graphs 


%  write_assignments/1
%    Write a list of assignments using write_assignment/1
%      Assignments - a list of assignments
%  write_assignments1/1
%    Auxiliary predicate; write three assignments per line
%  sort_assignments/2
%    By default, display a sorted list of assignments
%    Alternatively, display in reverse order of assignment
%  break_line/2
%    Line break after writing four assignments

write_assignments([]) :- !,
  write('[]').
write_assignments(A) :-
  sort_assignments(A, A1),
  write('['),
  write_assignments1(A1, 0),
  write(']').

write_assignments1([H], _) :- !,
  write_assignment(H, no).
write_assignments1([H|T], N) :-
  write_assignment(H, no),
  write(','),
  break_line(N, N1),
  write_assignments1(T, N1).

sort_assignments(A, A1) :-
  check_option(sorted), !,
  sort(A, A1).
sort_assignments(A, A).

break_line(7, 0) :- !,
  write('\n ').
break_line(N, N1) :- N1 is N + 1.

%  write_assignment/1
%    Write the assignment as Variable=Value@Depth/Antecedent
%    For an implication graph, the assignment can be 'kappa'
%  write_assignment/2
%    Called by write_assignment/1 (second argument is yes),
%      or directly with second argument no
%    Yes or no: write the antecedent or not
%  write_level/1
%    Write level and antecedent only if not dpll mode
%  write_antecedent/2
%    Write antecedent only if display option set

write_assignment(A) :-
  write_assignment(A, yes).

write_assignment(A, Antecedent) :-
  A = assign(V, N, _, _), 
  write(V), write('='), write(N),
  write_level(A),
  write_antecedent(A, Antecedent).
write_assignment(kappa, _) :-
  write(kappa).

write_level(assign(_, _, Depth, _)) :- 
  not_dpll_mode, !,
  write('@'),
  write(Depth).
write_level(_).

write_antecedent(assign(_, _, _, Unit), yes) :-
  not_dpll_mode,
  check_option(antecedent), !,
  write_antecedent1(Unit).
write_antecedent(_, _).

write_antecedent1(yes) :- !.
write_antecedent1(Unit) :-
  write('/'),
  write(Unit).
  
%  write_literal/1
%    Write a literal with a leading blank for positive literals
%      and a leading ~ for negative literals
write_literal(~Variable) :- !,
  write('~'),
  write(Variable).
write_literal(Variable) :-
  write(' '),
  write(Variable).
  

%  write_graph/1
%    graph(Nodes, Edges) - where Nodes and Edges are lists

write_graph(graph(Nodes, Edges)) :-
  write('[\n'),
  write_nodes(Nodes),
  write('\n]\n[\n'),
  write_edges(Edges),
  write('\n]').

%  write_nodes/1 - write the list of nodes as assignments

write_nodes([]).
write_nodes([N]) :- !,
  write_assignment(N, no).
write_nodes([N | Tail]) :-
  write_assignment(N, no),
  write(',\n'),
  write_nodes(Tail).

%  write_edges/1 - write the list of edges

write_edges([]).
write_edges([E]) :- !,
  write_arrow(E).
write_edges([E | Tail]) :-
  write_arrow(E),
  write(',\n'),
  write_edges(Tail).

%  write_arrow/2 - write the arrows, optionally with the clause

write_arrow(edge(From, Clause, To)) :-
  write_assignment(From, no),
  write(' --'),
  write(Clause),
  write('--> '),
  write_assignment(To, no).

%  write_paths/1
%    Write all paths from the decision node to kappa
%    Used when searching for a dominator
%      Paths - a list of paths

write_paths([]).
write_paths([Head | Tail]) :-
  write_one_path(Head),
  write_paths(Tail).

%  write_one_path/1
%    Write one path as node -> node -> ...

write_one_path([kappa]) :- !,
  write('kappa\n').
write_one_path([Assignment | Tail]) :-
  write_assignment(Assignment, no),
  write(' --> '),
  write_one_path(Tail).
