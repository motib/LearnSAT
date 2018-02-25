% Copyright 2012-17 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Set/clear display options and algorithm modes
%  Show the configuration
%  Show the "usage" message

:- module(modes, [
  set_display/1, clear_display/1, check_option/1,
  set_mode/1, set_look/1, alg_mode/1, not_dpll_mode/0, initialize/0,
  usage/0, display_header/0, show_config/0,
  set_decorate_mode/1, decorate_mode/1, look_mode/1]).

:- use_module([cdcl, config, counters, dot, io]).

% Algorithm and decorate modes (exported)

:- dynamic alg_mode/1, decorate_mode/1, look_mode/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    All initialization collected here

initialize :-
  init_mode,
  init_display,
  init_counters,
  init_tree,
  retractall(learned(_)),
  assert(learned([])),
  retractall(backtrack(_)),
  assert(backtrack(1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Algorithm and decorate modes

%  init_mode/0
%    If no modes assert the defaults from config

init_mode :-
  init_alg_mode,
  init_decorate_mode,
  init_look_mode.

init_alg_mode :-
  alg_mode(_), !.
init_alg_mode :-
  default_alg_mode(Default),
  assert(alg_mode(Default)).

init_decorate_mode :-
  decorate_mode(_), !.
init_decorate_mode :-
  default_decorate_mode(Default),
  assert(decorate_mode(Default)).

init_look_mode :-
  look_mode(_), !.
init_look_mode :-
  default_look_mode(Default),
  assert(look_mode(Default)).

%  LearnSAT can be run in three modes;
%    dpll -   DPLL algorithm
%    cdcl -   DPLL with conflict-directed clause learning (CDCL)
%    ncb  -   DPLL with CDCL and non-chronological backtracking (NCB)

%  set_mode/1 - Set a new algorithm mode

set_mode(Mode) :-
  check_mode(Mode), !,
  retractall(alg_mode(_)),
  assert(alg_mode(Mode)).
set_mode(_).

%  check_mode/1 - Check that the mode is legal or write error

check_mode(dpll).
check_mode(cdcl).
check_mode(ncb).
check_mode(X) :-
  write('Mode "'), write(X), write('" not recognized\n'),
  write('Run "usage" for a list of modes\n'),
  fail.

%    set_look/1
%      Lookahead for decision:
%      none
%      current  - max occurrences in current clauses
%      original - max occurrences in original clause (+ learned clauses) 

set_look(Look) :-
  check_look(Look), !,
  retractall(look_mode(_)),
  assert(look_mode(Look)).
set_look(_).

%  check_look/1 - Check that the lookahead mode is legal or write error

check_look(none).
check_look(current).
check_look(original).
check_look(X) :-
  write('Lookahead mode "'), write(X), write('" not recognized\n'),
  write('Run "usage" for a list of lookahead modes\n'),
  fail.

%  not_dpll_mode/0
%    Succeeds if not dpll mode

not_dpll_mode :-
  alg_mode(Mode),
  Mode \= dpll, !.


%  set_decorate_mode/1 - Set a new decorate mode

set_decorate_mode(Mode) :-
  check_decorate_mode(Mode), !,
  retractall(decorate_mode(_)),
  assert(decorate_mode(Mode)).
set_decorate_mode(_).


%  check_decorate_mode/1 - Check that the mode is legal or write error

check_decorate_mode(color).
check_decorate_mode(bw).
check_decorate_mode(X) :-
  write('Decorate mode "'), write(X), write('" not recognized\n'),
  write('Run "usage" for a list of decorate modes\n'),
  fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Display options
  
%  Database of display options
%    "none" is a dummy option to distinguish initialization (no options)
%    from the empty set of options

:- dynamic display_option/1.

%  Database of all options
%  The database of the default options is in file "config.pro"

all_display([
  antecedent, assignment, backtrack, clause, conflict, decision,
  dominator, dot, dot_inc, evaluate, graph, incremental, label,
  learned, look, none, partial, resolvent, result, skipped,
  sorted, tree, tree_inc, uip, unit, variable]).


%  init_display/0
%  If there are already display options, don't change them
%  Otherwise, set the default options

init_display :-
  display_header, nl,
  init_display1.

init_display1 :-
  display_option(_), !.
init_display1 :-
  default_display(List),
  set_display(List).


%  check_display/1
%    Check if a display option is set, otherwise fail

check_option(Option) :-
  display_option(Option).


%  set_display/1, clear_display/1
%    Set or clear display options
%    The argument can be "all", "default", a single option or
%      a list of options
%    For "all", "default", recurse with a list
%    For each option call set_display1/1, clear_display1/1
%  set_display1/1, clear_display1
%    Set or clear an individual option

set_display(all) :- !,
  all_display(List),
  set_display(List).

set_display(default) :- !,
  clear_display(all),
  default_display(List),
  set_display(List).

%  Default can also be (the first) element in a list of options
set_display([default | Tail]) :- !,
  set_display(default),
  set_display(Tail).

set_display([Head | Tail]) :- !,
  set_display1(Head),
  set_display(Tail).

set_display([]) :- !.

set_display(Option) :-
  set_display([Option]), !.

set_display(X) :-
  not_recognized(X).

%  Already set, don't do anything
set_display1(Option) :-
  display_option(Option), !.

%  Otherwise, assert it after checking that it exists in all_display
set_display1(Option) :-
  all_display(List),
  memberchk(Option, List), !,
  assert(display_option(Option)).

%  Otherwise, report that the option not recognized
set_display1(Option) :-
  not_recognized(Option).


clear_display(all) :- !,
  all_display(List),
  clear_display(List),
  set_display(none).

clear_display(default) :- !,
  default_display(List),
  clear_display(List).

clear_display([Head | Tail]) :- !,
  clear_display1(Head),
  clear_display(Tail).

clear_display([]).

clear_display(Option) :-
  clear_display([Option]), !.

clear_display(X) :-
  not_recognized(X).


%  If retract succeeds, the option is cleared
clear_display1(Option) :-
  retract(display_option(Option)), !.


%  If retract doesn't succeed, check that the option
%    exists in all_display
clear_display1(Option) :-
  all_display(List),
  memberchk(Option, List), !.


%  Otherwise, report that the option not recognized
clear_display1(Option) :-
  not_recognized(Option).


%  not_recognized/1
%    Print an error message if the option is not recognized

not_recognized(X) :-
  write('Display option "'), write(X), write('" not recognized\n'),
  write('Run "usage" for a list of options\n').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Display configuration and usage

%  show_config/0
%    Display version, default mode, learn mode and display options
%      and current mode, learn mode and display options

show_config :-
  display_copyright_notice,
  init_mode,
  default_alg_mode(M1),
  alg_mode(M2),
  write('Default mode: '),
  write(M1),
  write(', Current mode: '),
  write(M2), nl,
  default_look_mode(L1),
  look_mode(L2),
  write('Default lookahead mode: '),
  write(L1),
  write(', Current lookahead mode: '),
  write(L2), nl,
  write('Variable order:'),
  get_order(Order),
  write_order(Order), nl,
  default_display(D1),
  write('Default display options:\n'),
  sort(D1, D2),
  write(D2), nl,
  findall(D, display_option(D), List),
  write('Current display options:\n'),
  sort(List, List1),
  write(List1), nl,
  decorate_mode(D3),
  write('Decoration: '),
  write(D3), nl.
show_config.


%  write_order/1
%    Write the order of the variables if not the default lexicographic

write_order(default) :- !,
  write(' default').
write_order(Order) :-
  nl, write(Order).


%  usage/0 - print usage documentation
%  display_copyright_notice/0
%  display_header/0

display_header :-
  write('LearnSAT (version '), version(V), write(V), write(')').

display_copyright_notice :-
  display_header,
  write('. Copyright '), years(Y),   write(Y),
  write(' by Moti Ben-Ari. GNU GPL.\n').

usage :-
  display_copyright_notice, nl,
  write('dpll(Clauses, Decisions)\n'),
  write('  Clauses:   a list of list of literals (p or ~p)\n'),
  write('  Decisions: satisfying assignments or [] if unsatisfiable\n\n'),
  write('show_config\n'),
  write('  Show version, default and current mode and display options\n\n'),
  write('set_mode(Mode)\n'),
  write('  dpll:   DPLL algorithm (default)\n'),
  write('  cdcl:   DPLL with conflict-directed clause learning\n'),
  write('  ncb:    DPLL with CDCL and non-chronological backtracking\n\n'),
  write('set_look(Mode)\n'),
  write('  current:  max occurrences in current clauses\n'),
  write('  original: max occurrences in original (+learned) clauses\n'),
  write('  none:     no lookahead (default)\n\n'),
  write('set_decorate_mode(Mode)\n'),
  write('  color: color decorations (default)\n'),
  write('  bw:    black-and-white decorations\n\n'),
  write('set_order(Order)\n'),
  write('  default: variables assigned in lexicographical order\n'),
  write('  [...]:   variables assigned in the order [...]\n\n'),
  write('set_display(D), clear_display(D), where D can be a list\n'),
  write('  all          all the display options\n'),
  write('  default      default display options (marked *)\n\n'),
  write('  antecedent   antecedents of the implied literals\n'),
  write('  assignment   assignments that caused a conflict\n'),
  write('  backtrack *  level of non-chronological backtracking\n'),
  write('  clause       clauses to be checked for satisfiability\n'),
  write('  conflict *   conflict clauses\n'),
  write('  decision *   decision assignments\n'),
  write('  dominator    computation of the dominator\n'),
  write('  dot          implication graphs (final) in dot format\n'),
  write('  dot_inc      implication graphs (incremental) in dot format\n'),
  write('  graph        implication graphs (final) in textual format\n'),
  write('  incremental  implication graphs (incremental) in textual format\n'),
  write('  label        dot graphs and trees labeled with clauses\n'),
  write('  look         occurrences of variables in lookahead\n'),
  write('  learned *    learned clause by resolution\n'),
  write('  partial      partial assignments so far\n'),
  write('  resolvent *  resolvents created during CDCL\n'),
  write('  result *     result of the algorithm with statistics\n'),
  write('  skipped *    assignments skipped when backtracking\n'),
  write('  sorted *     assignments displayed in sorted order\n'),
  write('  tree         trees of assignments (final) in dot format\n'),
  write('  tree_inc     trees of assignments (incremental) in dot format\n'),
  write('  uip *        unique implication points\n'),
  write('  unit *       unit clauses\n'),
  write('  variable     variables that are not assigned so far\n').
