% Copyright 2017 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Ramsey's theorm
%  Satisfiable for K5, unsatisfiable for K6

:- use_module('../src/dpll').

% pij is true  if the edge from vertex i to vertex j
%   is colored blue
% pij is false if the edge from vertex i to vertex j
%   is colored red

ramsey5 :-
  dpll(
  [
  [x12,x23,x13],
  [~x12,~x23,~x13],

  [x12,x24,x14],
  [~x12,~x24,~x14],

  [x12,x25,x15],
  [~x12,~x25,~x15],

  [x13,x34,x14],
  [~x13,~x34,~x14],

  [x13,x35,x15],
  [~x13,~x35,~x15],

  [x14,x45,x15],
  [~x14,~x45,~x15],

  [x23,x34,x24],
  [~x23,~x34,~x24],

  [x23,x35,x25],
  [~x23,~x35,~x25],

  [x24,x45,x25],
  [~x24,~x45,~x25],

  [x34,x45,x35],
  [~x34,~x45,~x35]
  ], _).

ramsey6 :-
  dpll(
  [
  [x12,x23,x13],
  [~x12,~x23,~x13],

  [x12,x24,x14],
  [~x12,~x24,~x14],

  [x12,x25,x15],
  [~x12,~x25,~x15],

  [x12,x26,x16],
  [~x12,~x26,~x16],

  [x13,x34,x14],
  [~x13,~x34,~x14],

  [x13,x35,x15],
  [~x13,~x35,~x15],

  [x13,x36,x16],
  [~x13,~x36,~x16],

  [x14,x45,x15],
  [~x14,~x45,~x15],

  [x14,x46,x16],
  [~x14,~x46,~x16],

  [x15,x56,x16],
  [~x15,~x56,~x16],

  [x23,x34,x24],
  [~x23,~x34,~x24],

  [x23,x35,x25],
  [~x23,~x35,~x25],

  [x23,x36,x26],
  [~x23,~x36,~x26],

  [x24,x45,x25],
  [~x24,~x45,~x25],

  [x24,x46,x26],
  [~x24,~x46,~x26],

  [x25,x56,x26],
  [~x25,~x56,~x26],

  [x34,x45,x35],
  [~x34,~x45,~x35],

  [x34,x46,x36],
  [~x34,~x46,~x36],

  [x35,x56,x36],
  [~x35,~x56,~x36],

  [x45,x56,x46],
  [~x45,~x56,~x46]
  ], _).

