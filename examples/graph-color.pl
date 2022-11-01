% Copyright 2022 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- use_module('../src/dpll').


% K_22 is two-colorable
%
% pi is true  if vertex i is colored blue
% pi is false if vertex i is colored red

color_2_2 :-
  dpll(
  [
      [p1, p2], [~p1, ~p2],
      [p1, p4], [~p1, ~p4],
      [p3, p2], [~p3, ~p2],
      [p3, p4], [~p3, ~p4]
  ], _).

% K_3 is three-colorable
%
% bi is true  if vertex i is colored blue
% bi is false if vertex i is not colored blue
% ri is true if vertex i is colored red
% ri is false if vertex i is not colored red
% gi is true if vertex i is colored green
% gi is false if vertex i is not colored green

% Each vertex must be colored with some color
% If i is colored b, it cannot be colored r, etc.
% If i is colored b, i+1, i+2 cannot be colored b, etc.

color_3 :-
  dpll(
  [
    [b1, r1, g1], [~b1, ~r1], [~b1, ~g1], [~r1, ~g1],
    [b2, r2, g2], [~b2, ~r2], [~b2, ~g2], [~r2, ~g2],
    [b3, r3, g3], [~b3, ~r3], [~b3, ~g3], [~r3, ~g3],

    [~b1, ~b2], [~b1, ~b3], [~b2, ~b3],
    [~r1, ~r2], [~r1, ~r3], [~r2, ~r3],
    [~g1, ~g2], [~g1, ~g3], [~g2, ~g3]
  ], _).
