% Copyright 2017 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Schur triples
%  If duplicates are allowed, 4 may not have triples but 5 does
%  Without duplicates, 8 may not have triples but 9 does

:- use_module('../src/dpll').

schur4 :-
  dpll(
  [
  [x1a,x1b,x2a], [~x1a,~x1b,~x2a], [~x1a,x1b], [x1a,~x1b],
  [x2a,x2b,x4], [~x2a,~x2b,~x4], [~x2a,x2b], [x2a,~x2b],
  [x1a,x2a,x3], [~x1a,~x2a,~x3],
  [x1a,x3,x4], [~x1a,~x3,~x4]
  ], _).

schur5 :-
  dpll(
  [
  [x1a,x1b,x2a], [~x1a,~x1b,~x2a], [~x1a,x1b], [x1a,~x1b],
  [x2a,x2b,x4], [~x2a,~x2b,~x4], [~x2a,x2b], [x2a,~x2b],
  [x1a,x2a,x3], [~x1a,~x2a,~x3],
  [x1a,x3,x4], [~x1a,~x3,~x4],
  [x1a,x4,x5], [~x1a,~x4,~x5],
  [x2a,x3,x5], [~x2a,~x3,~x5]
  ], _).

schur8 :-
  dpll(
  [
  [x1,x2,x3], [~x1,~x2,~x3],
  [x1,x3,x4], [~x1,~x3,~x4],
  [x1,x4,x5], [~x1,~x4,~x5],
  [x1,x5,x6], [~x1,~x5,~x6],
  [x1,x6,x7], [~x1,~x6,~x7],
  [x1,x7,x8], [~x1,~x7,~x8],
  [x2,x3,x5], [~x2,~x3,~x5],
  [x2,x4,x6], [~x2,~x4,~x6],
  [x2,x5,x7], [~x2,~x5,~x7],
  [x2,x6,x8], [~x2,~x6,~x8],
  [x3,x4,x7], [~x3,~x4,~x7],
  [x3,x5,x8], [~x3,~x5,~x8]
  ], _).

schur9 :-
  dpll(
  [
  [x1,x2,x3], [~x1,~x2,~x3],
  [x1,x3,x4], [~x1,~x3,~x4],
  [x1,x4,x5], [~x1,~x4,~x5],
  [x1,x5,x6], [~x1,~x5,~x6],
  [x1,x6,x7], [~x1,~x6,~x7],
  [x1,x7,x8], [~x1,~x7,~x8],
  [x1,x8,x9], [~x1,~x8,~x9],
  [x2,x3,x5], [~x2,~x3,~x5],
  [x2,x4,x6], [~x2,~x4,~x6],
  [x2,x5,x7], [~x2,~x5,~x7],
  [x2,x6,x8], [~x2,~x6,~x8],
  [x2,x7,x9], [~x2,~x7,~x9],
  [x3,x4,x7], [~x3,~x4,~x7],
  [x3,x5,x8], [~x3,~x5,~x8],
  [x3,x6,x9], [~x3,~x6,~x9],
  [x4,x5,x9], [~x4,~x5,~x9]
  ], _).
