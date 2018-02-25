% Copyright 2012-17 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- use_module('../src/dpll').

%  Tseitin clauses for example in Section 4.5 of MLCS:
%  M. Ben-Ari. Mathematical Logic for Computer Science (Third Edition).
%  Springer, 2012.

ex :-
  dpll(
  [
  [~p, q], [p, ~q],
  [p, r, s], [~p, ~r, s], [~p, r, ~s], [p, ~r, ~s],
  [~s, t], [s, ~t],
  [~q, r, t], [q, ~r, t], [q, r, ~t], [~q, ~r, ~t]
	], _).
  
%  Satisfiable variant of the above formula

exs :-
  dpll(
  [
  [~p, q], [p, ~q],
  [p, r, s], [~p, ~r, s], [~p, r, ~s], [p, ~r, ~s],
  [~s, t], [s, ~t],
  [~q, r, t], [q, ~r, t], [q, r, ~t], [~q, ~r, t]
	], _).


%  Tseitin clauses for K_{2,2}  

k22 :-
  dpll(
  [
  [~p0, ~p1], [p0,  p1],
  [p0, ~p2], [~p0,  p2],
  [p1, ~p3], [~p1,  p3],
  [p2, ~p3], [~p2,  p3]
	], _).

%  Satisfiable variant of the above formula

k22s :-
  dpll(
  [
  [~p0, ~p1], [p0,  p1],
  [p0, ~p2], [~p0,  p2],
  [p1, ~p3], [~p1,  p3],
  [p2, ~p3], [~p2,  ~p3]
	], _).
  
%  Tseitin clauses for K_{3,3}

k33 :-
  dpll(
  [
        [~p0, ~p1,  p2], [~p0,  p1, ~p2], [p0, ~p1, ~p2],
	[p0,  p1,  p2],  [~p3,  p4,  p5], [p3, ~p4,  p5],
	[p3,  p4, ~p5],  [~p3, ~p4, ~p5], [~p6,  p7,  p8],
	[p6, ~p7,  p8],  [p6,  p7, ~p8],  [~p6, ~p7, ~p8],
	[~p0,  p3,  p6], [p0, ~p3,  p6],  [p0,  p3, ~p6],
	[~p0, ~p3, ~p6], [~p1,  p4,  p7], [p1, ~p4,  p7],
	[p1,  p4, ~p7],  [~p1, ~p4, ~p7], [~p2,  p5,  p8],
	[p2, ~p5,  p8],  [p2,  p5, ~p8],  [~p2, ~p5, ~p8]
	], _).

%  Satisfiable variant of the above formula

k33s :-
  dpll(
  [
        [~p0, ~p1,  p2], [~p0,  p1, ~p2], [p0, ~p1, ~p2],
	[p0,  p1,  p2],  [~p3,  p4,  p5], [p3, ~p4,  p5],
	[p3,  p4, ~p5],  [~p3, ~p4, ~p5], [~p6,  p7,  p8],
	[p6, ~p7,  p8],  [p6,  p7, ~p8],  [~p6, ~p7, ~p8],
	[~p0,  p3,  p6], [p0, ~p3,  p6],  [p0,  p3, ~p6],
	[~p0, ~p3, ~p6], [~p1,  p4,  p7], [p1, ~p4,  p7],
	[p1,  p4, ~p7],  [~p1, ~p4, ~p7], [~p2,  p5,  p8],
	[p2, ~p5,  ~p8], [p2,  p5, ~p8],  [~p2, ~p5, ~p8]
	], _).

k44 :-
  dpll(
  [
  [ e11, e12, e13, e14], 
  [~e11,~e12, e13, e14],
  [~e11, e12,~e13, e14], 
  [~e11, e12, e13,~e14], 
  [ e11,~e12,~e13, e14], 
  [ e11,~e12, e13,~e14], 
  [ e11, e12,~e13,~e14], 
  [~e11,~e12,~e13,~e14], 

  [~e21, e22, e23, e24],
  [ e21,~e22, e23, e24], 
  [ e21, e22,~e23, e24], 
  [ e21, e22, e23,~e24], 
  [~e21,~e22,~e23, e24], 
  [~e21,~e22, e23,~e24], 
  [~e21, e22,~e23,~e24], 
  [ e21,~e22,~e23,~e24], 

  [~e31, e32, e33, e34],
  [ e31,~e32, e33, e34], 
  [ e31, e32,~e33, e34], 
  [ e31, e32, e33,~e34], 
  [~e31,~e32,~e33, e34], 
  [~e31,~e32, e33,~e34], 
  [~e31, e32,~e33,~e34], 
  [ e31,~e32,~e33,~e34],

  [~e41, e42, e43, e44],
  [ e41,~e42, e43, e44], 
  [ e41, e42,~e43, e44], 
  [ e41, e42, e43,~e44], 
  [~e41,~e42,~e43, e44], 
  [~e41,~e42, e43,~e44], 
  [~e41, e42,~e43,~e44], 
  [ e41,~e42,~e43,~e44],

  [~e11, e21, e31, e41], 
  [ e11,~e21, e31, e41],
  [ e11, e21,~e31, e41], 
  [ e11, e21, e31,~e41], 
  [~e11,~e21,~e31, e41], 
  [~e11,~e21, e31,~e41], 
  [~e11, e21,~e31,~e41], 
  [ e11,~e21,~e31,~e41], 

  [~e12, e22, e32, e42],
  [ e12,~e22, e32, e42], 
  [ e12, e22,~e32, e42], 
  [ e12, e22, e32,~e42], 
  [~e12,~e22,~e32, e42], 
  [~e12,~e22, e32,~e42], 
  [~e12, e22,~e32,~e42], 
  [ e12,~e22,~e32,~e42], 

  [~e13, e23, e33, e43],
  [ e13,~e23, e33, e43], 
  [ e13, e23,~e33, e43], 
  [ e13, e23, e33,~e43], 
  [~e13,~e23,~e33, e43], 
  [~e13,~e23, e33,~e43], 
  [~e13, e23,~e33,~e43], 
  [ e13,~e23,~e33,~e43],

  [~e14, e24, e34, e44],
  [ e14,~e24, e34, e44], 
  [ e14, e24,~e34, e44], 
  [ e14, e24, e34,~e44], 
  [~e14,~e24,~e34, e44], 
  [~e14,~e24, e34,~e44], 
  [~e14, e24,~e34,~e44], 
  [ e14,~e24,~e34,~e44]
	], _).
