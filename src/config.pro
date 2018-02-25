% Copyright 2012-17 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Configuration
%    version, default algorithm mode, default display options
%    dot prologue, decorations and decorate mode

:- module(config,
  [version/1, years/1,
   default_alg_mode/1, default_display/1, default_decorate_mode/1,
   default_look_mode/1, dot_prologue/2, dot_decorate/3]).

version('2.0').

years('2012-17').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_alg_mode(dpll).

default_display(
  [backtrack, conflict, decision, learned,
  resolvent, result, skipped, sorted, uip, unit]).

default_look_mode(none).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_decorate_mode(color).

% dot prologue:
%   lr - left to right for implication graphs
%   tb - top to bottom for trees of assignments
dot_prologue(lr, 'digraph G {\n  rankdir=LR;\n').
dot_prologue(tb, 'digraph G {\n  rankdir=TB ranksep=equally;\n').

% dot decorations
%   There are versions for color and black-and-white

%   decision: decision assignment in tree and implication graph
%   decision_level: decision assignments with graph with dominator
%   dominator: dominator node and the kappa node
%   conflict: conflict leaf in tree
%   sat: leaf for satisfying assignment in a tree
%   cut: edge that is part of a cut

dot_decorate(color, decision,       ' [color="red"]').
dot_decorate(color, decision_level, ' [color="red"   peripheries="2"]').
dot_decorate(color, dominator,      ' [peripheries="2"]').
dot_decorate(color, conflict,       ' [color="red"   peripheries="2"]').
dot_decorate(color, sat,            ' [color="green" peripheries="2"]').
dot_decorate(color, cut,            ' color="blue"').

dot_decorate(bw, decision,       ' [style="bold"]').
dot_decorate(bw, decision_level, ' [style="bold" peripheries="2"]').
dot_decorate(bw, dominator,      ' [style="bold" peripheries="2"]').
dot_decorate(bw, conflict,       ' [style="bold" peripheries="2"]').
dot_decorate(bw, sat,            ' [style="bold" peripheries="3"]').
dot_decorate(bw, cut,            ' style="dashed"').
