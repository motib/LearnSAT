% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(dimacs, [op(610, fy,  ~), to_dimacs/3, from_dimacs/3]).

dynamic variable/1.

%  Convert a CNF formula in Prolog notation to and from DIMACS format
%    The Prolog notation is a list of clauses,
%    each clause is a list of literals,
%    a literal is an atom or the negation of an atom

%  to_dimacs(File, Comment, Clauses)
%    File name to write DIMACS format
%    Comment to appear in comments in file
%    Clauses to write

%  Get number of clauses and variables for the header
%  Then write the clauses

to_dimacs(File, Comment, Clauses) :-
  tell(File),
  length(Clauses, Number_of_Clauses),
  retractall(variable(_)),
  count_variables(Clauses, Variables),
  length(Variables, Number_of_Variables),
  write('c\nc '), write(Comment), write('\nc\np  cnf  '),
  write(Number_of_Variables), write('  '),
  write(Number_of_Clauses), nl,
  convert_clauses(Clauses),
  told.

%  count_variables(Clauses, Variables)
%    Return the set of variables (atomic propositions) in the Clauses
%    Traverse the clauses and assert each atom
%    Call setof to obtain a list with no duplicates

count_variables([], Variables) :-
  setof(X, variable(X), Variables),
  retractall(variable(_)).
count_variables([Head | Tail], Variables) :-
  count_variables1(Head),
  count_variables(Tail, Variables).

%  count_variables1(Clauses)
%    Assert all the atomic propositions of a single clause

count_variables1([]).
count_variables1([~ A | Tail]) :- !, 
  assert(variable(A)),
  count_variables1(Tail).
count_variables1([A | Tail]) :-
  assert(variable(A)),
  count_variables1(Tail).

% The DIMACS format expects atomic propositions to be number
%   so strip off any non-digits and write the atomic proposition
%   Use '-' for negation and '0' to terminate a clause

convert_clauses([]).
convert_clauses([Head | Tail]) :- 
  convert_clauses1(Head),
  convert_clauses(Tail).

convert_clauses1([]) :-
  write('0'), nl.
convert_clauses1([~ Head | Tail]) :- !,
  write('-'),
  convert_atom(Head),
  convert_clauses1(Tail).
convert_clauses1([Head | Tail]) :-
  convert_atom(Head),
  convert_clauses1(Tail).

%  Convert an atomic proposition to a character list and then
%    remove non-digits from the character list

convert_atom(Atom) :-
  atom_chars(Atom, Chars),
  only_digits(Chars, Chars1),
  atom_chars(Atom1, Chars1),
  write(Atom1), write(' ').

only_digits([], []).
only_digits([Head | Tail], [Head | Tail1]) :- 
  char_type(Head, digit), !,
  only_digits(Tail, Tail1).
only_digits([_ | Tail], Tail1) :- 
  only_digits(Tail, Tail1).


%  from_dimacs(Predicate, InFile, OutFile)
%    Read the DIMACS file and write as a set of Prolog clauses
%    Predicate for running dpll

from_dimacs(Predicate, InFile, OutFile) :-
  tell(OutFile),
  write(':- use_module(dpll).\n\n'),
  write(Predicate),
  write(' :-\n  dpll(\n'),
  see(InFile),
  read_line(Line),        % Initialize by reading first line
  process_line(Line, [], Clauses1),
  reverse(Clauses1, Clauses),
  write_clauses(Clauses),
  seen,
  write(', _).\n'),
  told.

%  read_line(List)
%    Read a line of characters from the input and return as a List
%    The list is built tail to head so reverse before returning
%  read_line1(Current, SoFar, List)
%    Auxiliary predicate for accumulating the line
%    Current is the character before a new one is read
%    It is used to remove extra blanks
%  check_end(Char, SoFar, List)
%    Check for end of line (return the list SoFar)
%      and end of file (return indication)

read_line(List) :-
  read_line1(' ', [], List1),
  reverse(List1, List).

read_line1(Current, SoFar, Line) :-
  get_char(C),
  check_end(Current, C, SoFar, Line).

check_end(_, end_of_file, _, [end_of_file]) :- !.
check_end(_, '\n', SoFar, SoFar)            :- !.
check_end(' ', ' ', SoFar, Line)            :- !,
  read_line1(' ', SoFar, Line).
check_end(_, C, SoFar, Line)                   :-
  read_line1(C, [C | SoFar], Line).

%  process_line(Line, SoFar, Clauses)
%    process a line of input by building and returning a clause as
%    a list of literals that is added to those created SoFar
%    Ignore ines starting with 'c' and 'p'

process_line(Line, SoFar, SoFar) :-
  Line = [end_of_file | _], !.
process_line(Line, SoFar, Clauses) :-
  Line = ['c' | _], !,
  read_line(Line1),
  process_line(Line1, SoFar, Clauses).
process_line(Line, SoFar, Clauses) :-
  Line = ['p' | _], !,
  read_line(Line1),
  process_line(Line1, SoFar, Clauses).
process_line(Line, SoFar, Clauses) :-
  to_clause(Line, Clause),
  read_line(Line1),
  process_line(Line1, [Clause | SoFar], Clauses).

%  to_clause(Line, List)
%    Interpret Line as a set of literals and return the List
%    Add 'p' to the number to create a Prolog atom

%  to_clause(Line, Temp, SoFar, List)
%     Use SoFar to accumulate the literals
%     Temp is a list of characters to transform into an atom

to_clause(Line, List) :-
  to_clause1(['p' | Line], [], [], List).

to_clause1(['0'], _, List, List) :- !.
to_clause1(['-' | Tail], ['p' | Temp], SoFar, List) :- !,
  to_clause1(Tail, ['p', '~' | Temp], SoFar, List).
to_clause1([' ' | Tail], Temp, SoFar, List) :- !,
  reverse(Temp, Temp1),
  atom_chars(Atom, Temp1),
  to_clause1(Tail, ['p'], [Atom | SoFar], List).
to_clause1([N | Tail], Temp, SoFar, List) :-
  to_clause1(Tail, [N | Temp], SoFar, List). 

%  write_clauses(Clauses)
%    Clauses - write the set of clauses

write_clauses([]) :- !,
  write('[]').
write_clauses(Clauses) :-
  write('[\n'),
  write_clauses1(Clauses),
  write(']').

write_clauses1([H]) :- !,
  write(H), nl.
write_clauses1([H|T]) :-
  write(H),
  write(',\n'),
  write_clauses1(T).
