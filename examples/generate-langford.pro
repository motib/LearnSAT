% Copyright 2017 by M. Ben-Ari. GNU GPL. See copyright.txt.

/*
  Generate SAT clauses for Langford's problem
  For n pairs, construct an array of n columns,
  with rows for every configuration of one number
  Construct clauses expressing:
    exactly one row for each number
    exactly one number in each column
  See LearnSAT tutorial for an example 
*/

%  Langford's problem for 3 pairs of numbers

generate :-
  tell('langford.pro'),
  writeln(":- use_module('../src/dpll')."), nl,
  generate3,
  nl, nl,
  generate4,
  told.

generate3 :-
  writeln("langford3 :- dpll(["),

  % The last/notlast parameter is used to prevent
  %   extra parentheses that the end of lists

  % Exactly one row for each of 1, 2, 3
  to_clauses([x1,x2,x3,x4], notlast),
  to_clauses([x5,x6,x7],    notlast),
  to_clauses([x8],          notlast),

  % Exactly one number in each column
  to_clauses([x1,x5,x8], notlast),
  to_clauses([x2,x6],    notlast),
  to_clauses([x1,x3,x7], notlast),
  to_clauses([x2,x4,x5], notlast),
  to_clauses([x3,x6,x8], notlast),
  to_clauses([x4,x7],    last),

  write(']\n,_ ).\n').


generate4 :-
  writeln("langford4 :- dpll(["),

  % The last/notlast parameter is used to prevent
  %   extra parentheses that the end of lists

  % Exactly one row for each of 1, 2, 3, 4
  to_clauses([x1,x2,x3,x4,x5,x6], notlast),
  to_clauses([x7,x8,x9,x10,x11],  notlast),
  to_clauses([x12,x13,x14,x15],   notlast),
  to_clauses([x16,x17],           notlast),

  % Exactly one number in each column
  to_clauses([x1,x7,x12,x16],    notlast),
  to_clauses([x2,x8,x13,x17],    notlast),
  to_clauses([x1,x3,x9,x14],     notlast),
  to_clauses([x2,x4,x7,x10,x15], notlast),
  to_clauses([x3,x5,x8,x11,x12], notlast),
  to_clauses([x4,x6,x9,x13,x16], notlast),
  to_clauses([x5,x10,x14,x17],   notlast),
  to_clauses([x6,x11,x15],       last),

  write(']\n,_ ).\n'),
  told.



% to_clause/2
%   List - The list of variables
%   Last - Is this the last set of clauses

to_clauses(List, Last) :-
  write('  ['),
  disjunction(List),
  writeln('],'),
  at_most(List, Last).

% disjunction/1
%   List - The list of variables
% At least one row must be chosen for each number 

disjunction([Head]) :-
  write(Head).
disjunction([Head | Tail]) :-
  write(Head), write(','),
  disjunction(Tail).

% At most one row must be chosen for each number
% Construct pairwise ~pi v ~pj for each 1<=i<j<=n

at_most([], _).
at_most([Head, Tail], Last) :- !,
  pair_of_literals([Tail], Head, Last).
at_most([Head | Tail], Last) :-
  pair_of_literals(Tail, Head, notlast),
  at_most(Tail, Last).

% pair_of_literals/3
%   List - the list of variables in positions beyond Head
%   Head - the first variable in the pair

pair_of_literals([], _, _).
pair_of_literals([L1|Tail], Head, Last) :-
  write('  [~'), write(Head), write(','),
  write('~'), write(L1), write(']'),
  write_last(Last), 
  pair_of_literals(Tail, Head, Last).

% write_list/1
%   Last - if last don't write comma
write_last(last) :- nl, !.
write_last(_) :- writeln(',').
