%
%
%                        LearnSat
%
%    Copyright 2012-17 by M. Ben-Ari. GNU GPL. See copyright.txt.


%  Davis-Putnam-Logemann-Loveland (DPLL) algorithm with
%    conflict-driven clause learning (CDCL),
%    non-chronological backtracking (NCB) and lookahead


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Export main predicate dpll/2 and the negation operator

:- module(dpll, [op(610, fy,  ~), dpll/2]).

%  Modules directly used by dpll
:- use_module([cdcl,counters,modes,display,dot,auxpred,io]).

%  Make user-interface predicates visible after consulting dpll
:- reexport(modes, 
  [show_config/0, usage/0, set_display/1, clear_display/1,
   set_mode/1, set_look/1, set_decorate_mode/1]).

:- reexport(auxpred, [set_order/1, get_order/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Data structures
%
%    Assignments:
%      assign(Variable, Value, Level, DecisionOrAntecedent)
%        DecisionOrAntecedent is 'yes' for decision assignments
%        and contains the antecedent assignment for implied assignments
%
%    Implication graph:
%      graph(Nodes, Edges)
%        A node is an assignment and an edge is a triple
%          edge(source assignment, clause, target assignment)
%
%    Learned clauses are stored as a dynamic list
%      learned(list of clauses) in cdcl.pro
%
%    Non-chronological backtracking level is stored as a dynamic integer
%      backtrack(Level) in cdcl.pro
%
%    List of variables in the clause is stored as a dynamic list
%      variables_list(list of variables) in auxpred.pro
%      to enable the user to change the order of the variables


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Main predicate dpll/2
%
%    Performs initialization and calls auxiliary predicate dpll/6,
%      which either finds a unit (find_unit) and propagates it
%      or makes a decision assignment (choose_assignment) and evaluates
%
%    The predicate ok_or_conflict is called with the result and
%      decides whether to recurse or fail
%
%    add_learned_clauses adds the clauses from the learned database
%      to the original set of clauses


%  dpll/2
%      Clauses   - set of clauses to be checked for satisfiability
%      Decisions - return assignments if the clauses are satisfiable
%                  or [] if the clauses are unsatisfiable

dynamic occurences/1.

dpll(Clauses, Decisions) :-
  initialize,
  display(clause, Clauses),
      % Create a list of the occurrences of variables
      %   in the clauses and a list with no duplicates
  get_variables_of_clauses(Clauses, Variables, Variables_No_Dups),
      % Initialize "original" lookahead mode
  set_original_lookahead(Clauses, Variables),
      % Call dpll/6, initially with Level 0, no assignments, empty graph
  dpll(Clauses, Variables_No_Dups, 0, [], graph([],[]), Decisions),
  display(result, Decisions).

%  dpll/6 failed so return unsatisfiable: empty list of assignments
dpll(_, []) :-
  display(result, []).

% set_original_lookahead/2
%   Compute the data base of counts of occurrences of variables
%     in the original set of clauses

set_original_lookahead(Clauses, Variables) :-
  look_mode(original), !,
  count_occurrences(Variables, Occurrences),
  retractall(occurrences(_)),
  assert(occurrences(Occurrences)),
  display(look, Occurrences, Clauses).
set_original_lookahead(_, _).


%  dpll/6
%      Clauses    - set of clauses
%      Variables  - list of unassigned variables
%      Level      - deepest decision level of assignments
%      SoFar      - assignments so far
%      Graph      - implication graph
%      Decisions  - return decisions

%  No more variables need to be assigned so the set is satisfiable
dpll(_, [], _, Decisions, _, Decisions) :- !,
  display(tree, Decisions, sat).

%  Attempt unit propagation
dpll(Clauses, Variables, Level, SoFar, Graph, Decisions) :-

      % Check for unit clauses, including the learned clauses
      % find_unit returns the clause that became a Unit and
      %   the Assignment forced by that unit clause
  add_learned_clauses(Clauses, All_Clauses),
  find_unit(All_Clauses, Level, SoFar, Unit, Assignment), !,

      % Add the new Assignment to the assignments SoFar
  New_Assignment = [Assignment | SoFar],
      % Increment the unit counter
  increment(unit),
  display(unit, Assignment, Unit),
  display(partial, New_Assignment),

      % Evaluate the set of Clauses using New_Assignment
      % Return the Result: (satisfied, not_resolved or conflict),
      %   and the Conflict clause if the result is a conflict
  evaluate(All_Clauses, New_Assignment, Conflict, New_Clauses, Result),
  display(clause, New_Clauses),

      % Get the new implication Graph1, adding the new Assignment
      %   that was forced by the unit clause
      % The antecedent that became a Unit labels the new edges
  extend_graph(Unit, Assignment, SoFar, Graph, New_Graph),
  display(incremental, New_Graph),
  display(dot_inc, New_Graph, Clauses),

      % Call ok_or_conflict with the Result of the evaluation,
      %   the Conflict clause, the New_Graph1 and add the New_Assignment
  ok_or_conflict(
    Result, Variables, All_Clauses,
    New_Assignment, Level, New_Graph, Conflict, Decisions).

%  Choose a decision assignment
dpll(Clauses, Variables, Level, SoFar, Graph, Decisions) :-
      % Increment the assignment Level and set the backtrack level to it
  New_Level is Level + 1,
  retract(backtrack(_)),
  assert(backtrack(New_Level)),
      % Choose a decision Assignment and increment the decision counter
  choose_assignment(Variables, Clauses, SoFar,
                    New_Level, Assignment),
      % Add the learned clauses
  add_learned_clauses(Clauses, All_Clauses),
  New_Assignment = [Assignment | SoFar],
  increment(decision),
  display(decision, Assignment),
  display(variable, Variables),
  display(partial, New_Assignment),

      % Evaluate the set of Clauses using the New_Assignment
      % Return the Result: (satisfied, not_resolved or conflict),
      %   and the Conflict clause if the result is a conflict
  evaluate(All_Clauses, New_Assignment, Conflict, New_Clauses, Result),
  display(clause, New_Clauses),

      % Call ok_or_conflict with the Result of the evaluation,
      %   the Conflict clause and the new Assignment
  ok_or_conflict(
    Result, Variables, All_Clauses,
    New_Assignment, New_Level, Graph, Conflict, Decisions).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  ok_or_conflict/8
%    Check the result of the evaluation
%        ok for satisfied or not_resolved, or conflict
%    Fail on conflict after learning a new clause
%
%      Status    - satisfied, not_resolved or conflict
%      Variables - list of unassigned variables
%      Clauses   - set of clauses
%      SoFar     - assignments so far
%      Level     - deepest decision level of assignments so far
%      Graph     - implication graph
%      Conflict  - conflict clause in case of conflict
%      Decisions - return decisions

%  Satisfied: call dpll with an empty list of variables to terminate
ok_or_conflict(satisfied, _, _, SoFar, _, _, _, Decisions) :-
   dpll(_, [], _, SoFar, _, Decisions).

%  Conflict: learn a clause (not in dpll mode) and fail
ok_or_conflict(conflict, _, Clauses, SoFar,
               Level, Graph, Conflict, _) :-
      % Increment the conflict counter
  increment(conflict),
  display(conflict, Conflict),
  display(assignment, SoFar),
  display(tree, SoFar, conflict),

      % Compute learned clauses only for not dpll mode
  not_dpll_mode,
      % Add the "kappa" node for the conflict clause to the graph
  extend_graph(Conflict, kappa, SoFar, Graph, Graph1),
      % Display the implication graph
  display(graph, Graph1),

      % Compute the learned clause and save in the database
  compute_learned_clause_by_dominator(Graph1, Level, Dominator),
  compute_learned_clause_by_resolution(Graph1, Conflict,
                                       SoFar, Level, Learned),
  update_variable_occurrences(Clauses),
      % Write the implication graph to the dot file
      %   using the Level and Dominator for emphasis
  display(dot, Graph1, Clauses, Level, Dominator, Learned),
      % Fail on conflict
  fail.

%  Not_resolved: delete the assigned variable from Variables
%      and recurse on dpll/6
ok_or_conflict(not_resolved, Variables, Clauses,
               SoFar, Level, Graph, _, Decisions) :-
  display(tree_inc, SoFar, ok),
  SoFar = [assign(V, _, _, _) | _],
  % Delete the variable
  selectchk(V, Variables, New_Variables),
  dpll(Clauses, New_Variables, Level, SoFar, Graph, Decisions).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  add_learned_clauses/2
%    Add the learned clauses to the set of clauses (not in dpll mode)

add_learned_clauses(Clauses, New_Clauses) :-
  not_dpll_mode, !,
  learned(Learned),
  union(Clauses, Learned, New_Clauses).
add_learned_clauses(Clauses, Clauses).

%  update_variables_occurrences/1
%    Update counts of variable occurrences to include learned clauses

update_variable_occurrences(Clauses) :-
  look_mode(original), !,
  add_learned_clauses(Clauses, All_Clauses),
  choose_variable_current(All_Clauses, [], Occurrences, _),
  retract(occurrences(_)),
  assert(occurrences(Occurrences)), !.
update_variable_occurrences(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Evaluate a set of clauses
%    The predicate is somewhat complex because it is used for:
%      finding a unit clause, evaluating a clause, for lookahead
%
%  evaluate/5
%    Evaluate a set of clauses
%        call auxiliary predicate evaluate1 adding more parameters   
%      Clauses     - the set of clauses
%      Assignments - the assignments used to evaluate the clauses
%      Conflict    - return a conflict clause
%      New_Clauses - the set of clauses after the evaluation
%                    used only for current lookahead
%      Result      - return: satisfied, not_resolved, conflict

evaluate(Clauses, Assignments, Conflict, New_Clauses, Result) :-
  evaluate1(Clauses, Assignments, satisfied, Conflict,
            [], New_Clauses, Result).

%  evaluate1/7
%      Clauses        - the set of clauses
%      Assignments    - the assignments used to evaluate the clauses
%      Current_Result - see below
%      Conflict       - return a conflict clause
%      New_Clauses    - the set of clauses after the evaluation
%                       used only for current lookahead
%                       the two parameters are an accumulator
%                       initialized to the empty list 
%      Result         - return: satisfied, not_resolved, conflict

%    Start by assuming satisfiable
%    If an unsatisfied clause return unsatisfied
%    If a non-satisfiable clause is found, return not_resolved

%  End of list of clauses, return satisfied or not_resolved
%    Return the accumulated list of New_Clauses
evaluate1([], _, not_resolved, _,
          New_Clauses, New_Clauses, not_resolved) :- !.
evaluate1([], _, satisfied, _,
          New_Clauses, New_Clauses, satisfied).

%  Evaluate the first clause to get a New_Clause and Result
%  Call evaluate2/9 adding these parameters
evaluate1([Head | Tail], Assignments, Current_Result, Conflict,
          New_Clauses, New_Clauses1, New_Result) :-
  evaluate_clause(Head, Assignments, New_Clause, Result),
  evaluate2(Result, [Head | Tail], Assignments, Current_Result,
            Conflict, New_Clause, New_Clauses, New_Clauses1,
            New_Result).

%  evaluate9/2
%    Auxiliary predicate to avoid repeated calls to evaluate_clause/4
%    Parameters are as in evaluate/7 plus the Result and
%      New_Clause from evaluate clause
      % If the clause is unsatisfied, return it as the Conflict clause
evaluate2(unsatisfied, [Head | _], _, _, Head, _, _, [], conflict) :- !.

      % If the clause is satisfied, recurse and ignore this clause 
evaluate2(satisfied, [_ | Tail], Assignments, Current_Result, Conflict,
          _, New_Clauses, New_Clauses1, Result1) :- !,
  evaluate1(Tail, Assignments, Current_Result, Conflict,
            New_Clauses, New_Clauses1, Result1).

      % If the clause is not_resolved, recurse and add the clause
      %   to the New_Clauses returned
evaluate2(_, [_ | Tail], Assignments, _, Conflict,
          New_Clause, New_Clauses, New_Clauses1, Result1) :- !,
  evaluate1(Tail, Assignments, not_resolved, Conflict,
            [New_Clause | New_Clauses], New_Clauses1, Result1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Find a unit clause
%    Fail if no unit clause
%
%  find_unit/5
%    Find a unit clause
%      Clauses    - set of clauses
%      Level      - decision level is needed to create an assignment
%      SoFar      - assignments made so far
%      Unit       - return the clause that becomes a unit
%      Assignment - return the assignment forced by the unit

%  Fail since no unit has been found by the end of the list of clauses
find_unit([], _, _, _, _) :- fail.

%  Evaluating the head of the list returns a single literal
%  Create an assignment from that literal
find_unit([Head | _], Level, SoFar, Head, Assignment) :-
  evaluate_clause(Head, SoFar, [Unit], Result),
  Result = unit, !,
    % Create non-decision assignment; Head is the antecedent clause
  to_assignment(Unit, Level, Head, Assignment).

%  The clause was not a unit, recurse on the rest of the clauses
find_unit([_ | Tail], Level, SoFar, Unit, Assignment) :-
  find_unit(Tail, Level, SoFar, Unit, Assignment).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  evaluate_clause/3
%    Evaluate a single clause
%      Clause      - the clause
%      Assignments - the assignments used to evaluate the clause
%      Result      - return satisfied, unsatisfied, unit, not_resolved

%  Evaluate the clause and translate the Result using
%    evaluate_return/4 to get one of the four results
evaluate_clause(Clause, Assignments, New1, Result1) :-
  evaluate_clause1(Clause, Assignments, notfound, New, Result),
  evaluate_return(Result, New, Result1, New1).


%  evaluate_clause1/5
%    Auxiliary predicate to avoid repeated calls to is_assigned/3
%      Clause      - the clause
%      Assignments - the assignments used to evaluate the clause
%      Found       - found an unassigned literal,
%                      initialized to notfound
%      New_Clause  - the new clause after the evaluation
%      Result      - return satisfied, unsatisfied, unit, not_resolved

%  All literals have been evaluated
evaluate_clause1([], _, found,     [], not_resolved).
evaluate_clause1([], _, notfound,  [], unsatisfied).
evaluate_clause1([], _, satisfied, [], satisfied).

%  If a literal is assigned 1 (true), terminate and return satisfied
evaluate_clause1([Head | _], Assignments, _, _, satisfied) :-
  is_assigned(Head, Assignments, 1), !,
  evaluate_clause1([], _, satisfied, _, satisfied).

%  If a literal is assigned 0 (false), recurse on the remaining literals
evaluate_clause1([Head | Tail], Assignments, Found, New, Result) :-
  is_assigned(Head, Assignments, 0), !,
  evaluate_clause1(Tail, Assignments, Found, New, Result).

%  If a literal not assigned, recurse on the remaining literals
%  Set Found to found and add the literal to the New_Clause
evaluate_clause1([Head | Tail], Assignments, _,
                 [Head | New_Clause], Result) :-
  evaluate_clause1(Tail, Assignments, found, New_Clause, Result).


% evaluate_return/4
%   Result   - of evaluate a clause
%   New      _ new clause after evaluation
%   Result1  - new result: not_resolved can also be unit
%   New1     _ new clause or [] for satisfied, unsatisfied

evaluate_return(satisfied,    _,   satisfied,   []).
evaluate_return(unsatisfied,  _,   unsatisfied, []).
evaluate_return(not_resolved, [X], unit, [X]) :- !.
evaluate_return(not_resolved, New, not_resolved, New).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Choose a decision assignment

%  choose_assignment/4
%      Variables  - list of unassigned variables for default choice
%      Clauses    - list of clauses for lookahead
%      Level      - record the decision level in the assignment
%                   also used for NCB
%      Assignment - return the decision assignment

%  Lookahead mode: max occurrences from current clauses
choose_assignment(_, Clauses, SoFar, Level, Assignment) :-
  look_mode(current), !,
  choose_variable_current(Clauses, SoFar, _, Variable),
  choose_assignment1(Variable, Level, Assignment).

%  Lookahead mode: max occurrences from original (+learned) clauses
choose_assignment(_, _, SoFar, Level, Assignment) :-
  look_mode(original), !,
  choose_variable_original(SoFar, Variable),
  choose_assignment1(Variable, Level, Assignment).

%  If no lookahead, choose first (unassigned) variable
choose_assignment([Variable | _], _, _, Level, Assignment) :-
  choose_assignment1(Variable, Level, Assignment).


%  choose_assignment1/3
%    Predicate common to default choice and lookahead
%      Variable   - the chosen variable
%      Level      - the decision level
%      Assignment - return the chosen assignment
 
choose_assignment1(V, Level, Assignment) :-
      % Choose a value for the assignment
  choose_value(N),
      % Build the Assignment term as a Decision assignment (yes)
  Assignment = assign(V, N, Level, yes),

      % Non-chronological backtracking when mode is ncb
      % If the current Level is greater than the backtrack level, fail
      % Cut within "if->then;else" is local and does not destroy the
      %   choice points for "member" and ";" above
  alg_mode(Mode),
  backtrack(L),
  (Mode = ncb, Level > L, Level > 1 ->
    display(skipped, Assignment), !, fail ;
    true).

%  choose_value/2
%      Variable - variable to assign to
%      N -        value returned 0 or 1

choose_value(0).
choose_value(1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  choose_variable_current/4
%    Lookahead to choose variable from current clauses
%    Choose the variable that appears most often in the clauses
%    Also return sorted list of occurence counts
%
%      Clauses     - the set of clauses
%      Assignments - current assignments
%      Sorted      - sorted list of occurrence counts 
%      Variable    - the chosen variable

choose_variable_current(Clauses, Assignments, Sorted, Variable) :-
    % Evaluate clauses to obtain the current set of clauses
  evaluate(Clauses, Assignments, _, New_Clauses, _),
    % Flatten the list of clauses to get all the literals
  flatten(New_Clauses, Literals),
    % Transform literals to variables
  literals_to_variables(Literals, [], Variables),
    % Count the occurrences of each variable
  count_occurrences(Variables, Sorted),
  display(look, Sorted, New_Clauses),
    % Return variable with most occurrences
  Sorted = [_-Variable | _].

% count_occurrences/2
%   List        - list of variable occurrences
%   Occurrences - list of pairs Count-Variable 

%  bagof returns lists [true, true, ...] with an atom "true"
%    for each occurrence of a Variable in List
%  findall return pairs Count-Variable, where Count is the length
%    of each list [true, true, ...] associated with Variable

%   Sort the list of occurrences using the first parameter
%     of a pair Count-Variable
%   @>= means sort descending, keeping duplicates

count_occurrences(List, Sorted_Occurrences):-
    findall(L-X, 
            (bagof(true,member(X,List),Xs), length(Xs,L)),
            Occurrences),
    sort(1, @>=, Occurrences, Sorted_Occurrences).

%  choose_variable_original/2
%    Lookahead to choose variable from original (+learned) clauses
%      The database "occurrences" contains a sorted list of occurences
%      Choose the first one _not_ already assigned
%
%      Assignments - current assignments
%      Variable    - the chosen variable

choose_variable_original(Assignments, Variable) :-
  occurrences(List),
  repeat,
    nth1(_, List, _-Variable),
    \+ memberchk(assign(Variable, _, _, _), Assignments), !.
