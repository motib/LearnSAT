% Copyright 2017 by M. Ben-Ari. GNU GPL. See copyright.txt.

/*
   Generate a program for showing van der Waerden's number for k = 3

   van der Waerden's number for three equally spaced
   sequences of the binary numbers 0, 1
   For n = 8, there are counterexamples
   For n = 9, all sequences have three equally spaced sequences
*/

%  Generate in file vdw.pro
generate :- 
  tell('vdw.pro'),
  writeln(":- use_module('../src/dpll')."),
  nl,
  generate(8,3),
  nl,
  generate(9,3),
  nl,
  generate(34,4),
  nl,
  generate(35,4),
  told.

%  generate/2
%    N - length of the sequence
%    K - number of equally spaced numbers  
generate(N, K) :-
  write("vdw"),
  write(N),
  writeln(" :- dpll(\n["),
  generate_clauses(N, K, 1, 1, 1, first),
  writeln('\n],_ ).').

%  generate_clauses/5
%    N       - length of the sequence
%    K       - number of equally spaced numbers
%    Start   - starting position (1) for each sequence
%    Current - current position in the sequence
%    D       - delta of the arithmetical sequence

generate_clauses(N, K, Start, Current, D, First) :-
  (First == first -> true; writeln(',')), 
  write('['),
  % Generate the clause with positive literals
  generate_one_clause(N, K, 1, Current, D, pos),
  write(','), write('['),
  % Generate the clause with negative literals
  generate_one_clause(N, K, 1, Current, D, neg),
  % Increase the starting position for the next arithemtic sequence
  Start1 is Start + 1,
  % Compute the end position given K and D
  %   and check if it is beyond the length of the sequence
  End is Start1 + (K-1)*D,
  End =< N, !,
  % Recurse with the new starting position
  generate_clauses(N, K, Start1, Start1, D, notfirst).

generate_clauses(N, K, _, _, D, First) :-
  % If this sequence is finished, increase the delta
  D1 is D + 1,
  % Compute the end position given K and D
  %   and check if it is beyond the length of the sequence
  End is 1 + (K-1)*D1,
  End =< N, !,
  % Recurse from start position 1 and new delta
  generate_clauses(N, K, 1, 1, D1, First).

generate_clauses(_, _, _, _, _, _).


%  generate_one_clause/6
%    N       - length of the sequence
%    K       - number of equally spaced numbers
%    Accum   - accumulator of number of values produced (initially 1)
%    Current - current position in the sequence
%    D       - delta of the arithmetical sequence
%    Neg     - neg = negate the sequence
generate_one_clause(N, K, Accum, Current, D, Neg) :-
  % Check if number of literals is less that the number of
  %   equally spaced numbers 
  Accum < K, !,
  % Write the negation symbol if specified
  (Neg == neg -> write('~') ; true),
  % Write the value of the literal
  write('x'), write(Current), write(','),
  % Increase the current position and the number of literals
  Current1 is Current + D,
  Accum1 is Accum + 1,
  % Recurse with new position and number of literals
  generate_one_clause(N, K, Accum1, Current1, D, Neg).

generate_one_clause(_, _, _, Current, _, Neg) :-
   % Prevent printing of comma after the last literal
  (Neg == neg -> write('~') ; true),
  write('x'), write(Current), write(']').
