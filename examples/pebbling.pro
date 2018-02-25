% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- use_module('../src/dpll').

%  Grid pebbling

grid2 :-
  dpll(
  [
    % target clauses
    [~p1], [~p2],      
    % precedence clauses
    [~q1, ~r1, p1, p2],   
    [~q1, ~r2, p1, p2],
    [~q2, ~r1, p1, p2],
    [~q2, ~r2, p1, p2],
     % source clauses
    [q1, q2], [r1, r2]
    
  ], _).

%  dpll: units=74, decisions=50, conflicts=26
%  cdcl: units=25, decisions=14, conflicts=8
%  ncb:  units=17, decisions=9,  conflicts=3

grid3 :-
  dpll(
  [
    % target clauses
    [~p1], [~p2],
    % precedence clauses
    [~q1, ~r1, p1, p2],
    [~q1, ~r2, p1, p2],
    [~q2, ~r1, p1, p2],
    [~q2, ~r2, p1, p2],
    [~s1, ~t1, q1, q2],
    [~s1, ~t2, q1, q2],
    [~s2, ~t1, q1, q2],
    [~s2, ~t2, q1, q2],
    [~t1, ~u1, r1, r2],
    [~t1, ~u2, r1, r2],
    [~t2, ~u1, r1, r2],
    [~t2, ~u2, r1, r2],
    % source clauses
    [s1, s2], [t1, t2], [u1, u2]
  ], _).
  
bottom_to_top :-
  set_order([t1,t2,u1,u2,q1,q2,r1,r2,s1,s2,p1,p2]).

