%    Copyright 2012-17 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(cdcl,
     [extend_graph/5,
     compute_learned_clause_by_resolution/5,
     compute_learned_clause_by_dominator/3,
     learned/1, backtrack/1]). 

:- use_module([auxpred, display, modes,io]).

:- dynamic learned/1, backtrack/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  extend_graph/5
%    Extend the implication graph for a unit clause
%    The assignment implied by the unit clause will be a new target node
%    Add edges from each assignment to the other literals in the clause
%    The edges are labeled with the number of this clause
%        Unit       - a clause determined to be unit
%        Assignment - current assignment
%        SoFar      - assignments so far
%        Graph      - current implication graph
%        Graph1     - return the new implication graph

extend_graph(Unit, Assignment, SoFar, Graph, Graph1) :-
  not_dpll_mode, !,
  extend_graph1(Unit, Unit, Assignment, SoFar, Graph, Graph1).
extend_graph(_, _, _, Graph, Graph).


%  If the Assignment is to this literal, don't add to graph
extend_graph1([Head | Tail], Unit, Assignment, SoFar, Graph, Graph1) :-
  to_variable(Head, Variable),
  Assignment = assign(Variable, _, _, _), !, 
  extend_graph1(Tail, Unit, Assignment, SoFar, Graph, Graph1).

%  Otherwise, search for an assignment to this literal
%    which will become a new source node with an edge to Assignment
extend_graph1([Head | Tail], Unit, Assignment, SoFar, 
          graph(Nodes, Edges), Graph1) :-
  to_variable(Head, Variable),
  Source = assign(Variable, _, _, _),
  memberchk(Source, SoFar), !,
      % If found, add this assignment as a source node
      % and add the antecedent unit clause as a new edge
  extend_graph1(Tail, Unit, Assignment, SoFar,
    graph([Source | Nodes],
          [edge(Source, Unit, Assignment) | Edges]), Graph1).

%  End of the clause, add Assignment as a target node
%  Sort the nodes and edges (this removes duplicates, if any)
extend_graph1([], _, Assignment, _, 
          graph(Nodes, Edges), graph(Nodes1, Edges1)) :- !,
  sort([Assignment | Nodes], Nodes1),
  sort(Edges, Edges1).

extend_graph1(_, _, _, _, Graph, Graph).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Compute a learned clause by resolving backwards from the conflict

%  compute_learned_clause_by_resolution/5
%    Compute a learned clause from an implication graph
%        by resolving backwards from the conflict clause
%      Graph   - implication graph (for computing the backtrack level)
%      Clause  - the conflict clause
%      SoFar   - the set of assignments so far
%      Level   - the current (highest) level
%      Learned - the learned clause for displaying the cut

compute_learned_clause_by_resolution(
      graph(Nodes, _), Clause, SoFar, Level, Learned) :- !,
  learn_clause_from_antecedents(Clause, SoFar, Level, Learned),
  display(learned, Learned),
      % If learn mode is resolution, compute the backtrack level
      %   and the learned clause to the list of learned clauses 
  compute_backtrack_level(Learned, Level, 0, Nodes),
  retract(learned(List)),
  union([Learned], List, List1),
  assert(learned(List1)).
compute_learned_clause_by_resolution(_, _, _, _, _).


%  learn_clause_from_antecedents/4
%    Construct the learned clause from the antecedents
%      Clause      - the current clause
%      Assignments - the assignments so far
%      Level       - the current (highest) level
%      Learned     - return the learned clause

%  Terminate if a unique implication point (uip) has been reached
%    The current clause becomes the learned clause
learn_clause_from_antecedents(Learned, Assignments, Level, Learned) :-
  check_uip(Learned, Assignments, Level, []), !.

%  Resolve the current clause (if possible) with the antecedent
%    clause of the last assignment
learn_clause_from_antecedents(Clause, [Last | Rest], Level, Learned) :-
  to_literal(Last, Literal),
  to_complement(Literal, Complement),
  memberchk(Complement, Clause),
  Last = assign(_, _, _, Antecedent),
  Antecedent \= yes, !,
      % Resolve and recurse
  resolve(Complement, Clause, Antecedent, Resolvent),
  display(resolvent, Clause, Antecedent, Resolvent),
  learn_clause_from_antecedents(Resolvent, Rest, Level, Learned).


%  check_uip/4
%    A UIP has exactly one literal assigned at the current level
%      Clause      - the current clause
%      Assignments - the set of assignments so far
%      Level       - the current (highest) level
%      Number      - the set of literals assigned at this level

%  If two literals are assigned at this level, the clause is not a uip
check_uip(_, _, Level, Literals) :-
  length(Literals, 2), !,
  display(uip, no, Literals, Level),
  fail.

%  End of the clause reached
%  If one literal is assigned at this level, the clause is a uip
check_uip([], _, Level, Literals) :-
  length(Literals, 1), !,
  display(uip, yes, Literals, Level).

%  Check if the next literal is assigned at this level
%  If so, increment the number of literals assigned and recurse
check_uip([Head | Tail], Assignments, Level, Literals) :-
  to_variable(Head, Variable),
  memberchk(assign(Variable, _, Level, _), Assignments), !,
  display(literal, Head, Level),
  check_uip(Tail, Assignments, Level, [Head | Literals]).

% The variable of the literal was not assigned at this level, recurse
check_uip([_ | Tail], Assignments, Level, Literals) :-
  check_uip(Tail, Assignments, Level, Literals).


%  resolve/4
%    Resolve two clauses on a given literal
%      Literal   - literal from Clause1 to resolve on
%      Clause1   - first clause
%      Clause2   - second clause with complement of Literal
%      Resolvent - return resolvent clause
resolve(Literal, Clause1, Clause2, Resolvent) :-
  delete(Clause1, Literal, Clause11),
  to_complement(Literal, Literal1),
  delete(Clause2, Literal1, Clause21),
  union(Clause11, Clause21, Resolvent).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Compute a learned clause by locating a dominator

%  compute_learned_clause_by_dominator/3
%    Graph - the implication graph
%    Level - the highest level in the graph
%    Dominator - return the dominator for emphasis on dot graph

compute_learned_clause_by_dominator(graph(Nodes, Edges), Level, Dominator) :-
  not_dpll_mode,
  check_option(dominator), !,
      % Get paths from the highest decision assignment to kappa
  get_paths_to_kappa(Nodes, Edges, Level, Level, Paths_From_Highest),
      % Find a dominator for the paths
  get_dominator(Paths_From_Highest, Dominator),
      % Find all paths from decision nodes with lower levels to kappa
  Level1 = Level - 1,
  get_paths_to_kappa(Nodes, Edges, 1, Level1, Paths_From_Lower),
      % Remove paths that go through the dominator
  remove_dominators(Dominator, Paths_From_Lower, [], Paths_No_Dominator),
      % Get edges that cross the cut: from lower levels to highest level
  get_edges_over_cut(Paths_No_Dominator, Level, Assignments_Into_Cut),
      % Add the assignments at the source of these edges to the dominator
  union(Assignments_Into_Cut, [Dominator], Learned_Assignments),
      % Convert to clause form and complement each literal
  to_complemented_clause(Learned_Assignments, Learned),
  display(dominator, Paths_From_Highest, Dominator, Paths_No_Dominator, Learned).
compute_learned_clause_by_dominator(_, _, no).


%  get_paths_to_kappa/5
%    get all paths from decision nodes to kappa
%      Nodes, Edges - of the implication graph
%      Level of nodes has to be between From and To
%      Paths - return the list of paths

get_paths_to_kappa([Head | Tail], Edges, From, To, Paths) :-
  Head = assign(_, _, Level, yes),
  From =< Level, Level =< To, !,
  findall(Path, get_path(Edges, Head, kappa, [Head], Path), Paths1),
  get_paths_to_kappa(Tail, Edges, From, To, Paths2),
  append(Paths1, Paths2, Paths).
get_paths_to_kappa([_ | Tail], Edges, From, To, Paths) :-
  get_paths_to_kappa(Tail, Edges, From, To, Paths).
get_paths_to_kappa([], _, _, _, []).


%  get_path/5
%    Get a path in the implication graph
%    Since the graph is a dag, a transitive computation suffices
%      Edges  - the edges of the graph
%      Source - the source node
%      Target - the target node
%      So_Far - the path so far
%      Path   - the path that is returned

%  When the Source equals the target the path has been found
%  Reverse the list so that the source comes first
get_path(_, Target, Target, So_Far, Path) :-
  reverse(So_Far, Path).
get_path(Edges, Source, Target, So_Far, Path) :-
  member(edge(Source, _, Next), Edges),
  get_path(Edges, Next, Target, [Next | So_Far], Path).


%  get_dominator/3, get_dominator1/3
%    Dominators are nodes which appear in all paths
%      from the source decision assignment to kappa
%      but is not kappa
%    Take the intersection of all the path lists
%    Return the last node in the list of dominators
%      Path_List  - the list of paths
%      Dominator  - the dominator that is returned

get_dominator([Head | Tail], Dominator) :-
  get_dominator1(Tail, Head, Result),
  subtract(Result, [kappa], Result1),
  reverse(Result1, [Dominator | _]).

get_dominator1([Head | Tail], So_Far, Result) :-
  intersection(Head, So_Far, So_Far1),
  get_dominator1(Tail, So_Far1, Result).
get_dominator1([], So_Far, So_Far).


%  remove_dominators/4
%    Remoeve paths that contain the dominator
%      Dominator - the dominator node
%      Paths - the list of paths
%      So_Far - paths without dominator so far
%      Result - paths without dominator

remove_dominators(_, [], Result, Result).
remove_dominators(Dominator, [Head | Tail], So_Far, Result) :-
  memberchk(Dominator, Head), !,
  remove_dominators(Dominator, Tail, So_Far, Result).
remove_dominators(Dominator, [Head | Tail], So_Far, Result) :-
  remove_dominators(Dominator, Tail, [Head | So_Far], Result).


%  get_edges_over_cut/3
%    From a set of paths from lower decision levels to kappa
%    return the edges that cross from lower levels to higher levels 
%      Paths - the set of paths
%      Level - the highest level
%      Assignments - return the assignments
%  edge_over_cut/3
%    Search for a node at the highest level and check if the previous
%    node in the path (if any) is at a lower level 

get_edges_over_cut(Paths, Level, Set_Of_Assignments) :-
   findall(A, edge_over_cut(Paths, Level, A), Assignments),
   list_to_set(Assignments, Set_Of_Assignments).

edge_over_cut(Paths, Level, Assignment) :-
  member(Path, Paths),
  nth1(N, Path, assign(_, _, Level, _)),
  N > 1,
  N1 is N - 1,
  nth1(N1, Path, Assignment),
  Assignment = assign(_, _, Level1, _),
  Level1 < Level.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  compute_backtrack_level/4
%    Compute the non-chronological backtrack level as the highest
%      level of an assignment for the learned clause
%      except for the current level
%        Learned  - learned clause
%        Level    - current level
%        Highest  - highest level so far
%        Nodes    - nodes with assignments in the implication graph

%  At end of learned clause, save the highest level
compute_backtrack_level([], Level, Highest, _) :-
  retract(backtrack(_)),
  assert(backtrack(Highest)),
  Highest < Level, !,
  display(backtrack, Highest).

compute_backtrack_level([], _, _, _).

%  Check each literal of the learned clause
%  Search for an assignment to the complement of the literal
%  Save if higher than seen so far,
%  but not the current level unless the assignment at the current
%  level is a decision assignment

compute_backtrack_level([Head | Tail], Level, Highest, Nodes) :-
  to_complement(Head, Head1),
  to_assignment(Head1, L, _, Assignment),
  memberchk(Assignment, Nodes),
  L > Highest,
  not_current_level(Assignment, Level), !,
  compute_backtrack_level(Tail, Level, L, Nodes).

%  Otherwise, recurse
compute_backtrack_level([_ | Tail], Level, Highest, Nodes) :-
  compute_backtrack_level(Tail, Level, Highest, Nodes).


%  not_current_level/2
%      Assignment - an assignment
%      Level - current level
%    Succeed if level of assignment is not equal to current Level
%    or if it is equal and the assigment is a decision assignment

not_current_level(assign(_, _, L, _),       Level) :- L =\= Level, !.
not_current_level(assign(_, _, Level, yes), Level).
