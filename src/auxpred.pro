% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(auxpred,
  [is_assigned/3, literals_to_variables/3, get_variables_of_clauses/3,
   to_variable/2, to_complement/2, to_assignment/4,
   to_literal/2, to_complemented_clause/2, set_order/1, get_order/1]).  

:- use_module([counters]).

:- op(610, fy,  ~).

:- dynamic variables_list/1.

%  Auxiliary predicates concerned with assignments, variables, literals

%  is_assigned/3
%    Check if a literal is assigned and if so return its value
%    Fail if not assigned
%      Literal      - check if this literal is assigned
%      Assignments  - the assignments to be checked
%      Value        - return the value of the literal

    % The literal is a negated assigned atom; return its complement
is_assigned(~ Variable, Assignments, Value1) :- !,
  memberchk(assign(Variable, Value, _, _), Assignments),
  Value1 is 1-Value.
    % The literal is an assigned atom; return the value
is_assigned(Variable, Assignments, Value) :-
  memberchk(assign(Variable, Value, _, _), Assignments).


%  get_variables_of_clauses/3
%    Get the list of occurrences of variables from a set of clauses
%      and the list of variables with duplicates removed
%    The predicate sort removes duplicates

get_variables_of_clauses(Clauses, Variables_List, Variables_No_Dups) :-
  flatten(Clauses, Literals_List),
  literals_to_variables(Literals_List, [], Variables_List),
  sort(Variables_List, Sorted_Variables_List),
  order_variables(Sorted_Variables_List, Variables_No_Dups),
  length(Clauses, Number_of_Clauses),
  length(Variables_No_Dups, Number_of_Variables),
  init_input_counters(Number_of_Clauses, Number_of_Variables).

%  literals_to_variables/3
%    Get the set of variables of a list of literals
%      Literals      - a list of literals
%      SoFar         - the variables found so far
%      Variables_Set - the set of variables of these literals

literals_to_variables([], Variables_List, Variables_List).
literals_to_variables([V | Tail], SoFar, Variables_List) :-
  to_variable(V, V1),
  literals_to_variables(Tail, [V1 | SoFar], Variables_List).


%  to_variable/2
%    Get the variable of a literal
%      Literal  - a literal
%      Variable - the variable of that literal

to_variable(~ Variable, Variable) :- !.
to_variable(Variable,   Variable).


%  to_complement/2
%    Get the complement of a literal
%      Literal  - a literal
%      Literal1 - the complement of Literal
%    Called with kappa in decorate_cut

to_complement(kappa, kappa) :- !.
to_complement(~ Variable, Variable) :- !.
to_complement(Variable,   ~ Variable).


%  to_assignment/4
%    Constructor for the term assign/4
%      Literal    - a literal
%      Level      - a decision level
%      Decision   - is this a decision assignment?
%                   yes or antecedent clause
%      Assignment - the literal expressed as an assignment

to_assignment(~ Variable, Level, Decision,
              assign(Variable, 0, Level, Decision)) :- !.
to_assignment(Variable,   Level, Decision,
              assign(Variable, 1, Level, Decision)).


%  to_literal/2
%    Assignment - an assignment
%    Literal    - the assignment as a literal
%  Example: assign(p1, 0, 3, no) becomes ~p1
%  Called with kappa in decorate_cut

to_literal(assign(V, 0, _, _), ~V).
to_literal(assign(V, 1, _, _), V).
to_literal(kappa, kappa).

%  to_complemented_clause/2
%    List of assignments to a list of complemented literals
%      Assignments - a list of assignments
%      Clause      - the corresponding clause

to_complemented_clause([], []).
to_complemented_clause([Head1 | Tail1], [Head2 | Tail2]) :-
  to_complemented_literal(Head1, Head2),
  to_complemented_clause(Tail1, Tail2).

%  to_complemented_literal/2
%    Assignment - an assignment
%    Literal    - the assignment as a literal after complementing
%  Example: assign(p1, 1, 3, no) becomes ~p1

to_complemented_literal(assign(V, 0, _, _), V).
to_complemented_literal(assign(V, 1, _, _), ~V).


%     Order of assignment to variables
%  set_order/1
%    Variables are assigned in the order of their appearance in List
%    Return order to display configuration

set_order(default) :- !,
  retractall(variables_list(_)).

set_order(List) :-
  retractall(variables_list(_)),
  assert(variables_list(List)).

get_order(List) :-
  variables_list(List), !.
get_order(default).

% order_variables/2
%   Re-order the list of variables if requested (set_order)
%   Check that the request list is a permutation of the
%     actual variables

order_variables(Variables_List, Variables) :-
  variables_list(Variables),
  permutation(Variables_List, Variables), !.

order_variables(_, Variables) :-
  variables_list(Variables), !,
  write('Ordered list of variables not a permutation of the variables in the clauses\n'),
  abort.
order_variables(Variables_List, Variables_List).
