% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Examples from published papers
%  LearnSAT makes decision assignments lexicographically
%    so literals have been renamed to force the order in those papers

:- use_module('../src/dpll').

%  Example from Marques-Silva, Lynce, Malik in the Handbook

% dpll: units=9,  decisions=9, conflicts=2
% cdcl: units=8,  decisions=6, conflicts=1
% ncb:  units=10, decisions=6, conflicts=1

mlm :-
  dpll(
  [
  [x1, x031, ~x2], [x1, ~x3], [x2, x3, x4],
  [~x4, ~x5], [x021, ~x4, ~x6], [x5, x6]
  ], _).


%  Example from Malik-Zhang paper in CACM 52(8), 2009.
%  Figure 3 states: (~x3 v ~x7 v ~x8) is valid
%  Should be: the learned clause (~x3 v ~x7 v x8) is false
%    under this assignment

mz :-
  set_order([x1,x3,x2,x7,x4,x8,x9,x10,x11,x12]),
  dpll(
  [
  [x1, x4], [x1, ~x3, ~x8], [x1, x8, x12], [x2, x11],
  [~x7, ~x3, x9], [~x7, x8, ~x9], [x7, x8, ~x10],
  [x7, x10, ~x12]
  ], _),
  set_order(default).


%  Example from Marques-Silva and Sakallah GRASP paper

ms :-
  dpll(
  [
  [x1, x2], [x1, x3, ax9], [~x2, ~x3, x4], [~x4, x5, bx10],
  [~x4, x6, cx11], [~x5, ~x6], [~x1, x7, dx12], [~x1, x8],
  [~x7, ~x8, dx13]
  ], _).
