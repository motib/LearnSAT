% Copyright 2014 by N. Tal and N. Gilboa. GNU GPL. See copyright.txt.

% Seating problem around a table with 6 chairs (version c)

%  1 won't sit next to 2
%  3 must  sit next to 4
%  4 must  sit next to 5
%  5 must  sit next to 6 

% Unsatisfiable with:
%   clauses=126, variables=36
%   dpll: units=1187, decisions=286, conflicts=144
%   cdcl: units=1016, decisions=232, conflicts=117
%   ncb : units=926, decisions=207, conflicts=92

%  pij: i is sitting in the j'th chair

:- use_module('../src/dpll').

table :-
  dpll([

   % Every person sits in some chair.

  [p11, p12, p13, p14, p15, p16], [p21, p22, p23, p24, p25, p26], [p31, p32, p33, p34, p35, p36], 
  [p41, p42, p43, p44, p45, p46], [p51, p52, p53, p54, p55, p56], [p61, p62, p63, p64, p65, p66],

  % No more than one person sits in any chair 

  % Chair 1 
  [~p11, ~p21], [~p11, ~p31], [~p11, ~p41], [~p11, ~p51], [~p11, ~p61], 
  [~p21, ~p31], [~p21, ~p41], [~p21, ~p51], [~p21, ~p61],  
  [~p31, ~p41], [~p31, ~p51], [~p31, ~p61], 
  [~p41, ~p51], [~p41, ~p61], 
  [~p51, ~p61], 

  % Chair 2  
  [~p12, ~p22], [~p12, ~p32], [~p12, ~p42], [~p12, ~p52], [~p12, ~p62], 
  [~p22, ~p32], [~p22, ~p42], [~p22, ~p52], [~p22, ~p62],  
  [~p32, ~p42], [~p32, ~p52], [~p32, ~p62], 
  [~p42, ~p52], [~p42, ~p62], 
  [~p52, ~p62], 

  % Chair 3
  [~p13, ~p23], [~p13, ~p33], [~p13, ~p43], [~p13, ~p53], [~p13, ~p63], 
  [~p23, ~p33], [~p23, ~p43], [~p23, ~p53], [~p23, ~p63],  
  [~p33, ~p43], [~p33, ~p53], [~p33, ~p63], 
  [~p43, ~p53], [~p43, ~p63], 
  [~p53, ~p63], 

  % Chair 4 
  [~p14, ~p24], [~p14, ~p34], [~p14, ~p44], [~p14, ~p54], [~p14, ~p64], 
  [~p24, ~p34], [~p24, ~p44], [~p24, ~p54], [~p24, ~p64],  
  [~p34, ~p44], [~p34, ~p54], [~p34, ~p64], 
  [~p44, ~p54], [~p44, ~p64], 
  [~p54, ~p64], 

  % Chair 5 
  [~p15, ~p25], [~p15, ~p35], [~p15, ~p45], [~p15, ~p55], [~p15, ~p65], 
  [~p25, ~p35], [~p25, ~p45], [~p25, ~p55], [~p25, ~p65],  
  [~p35, ~p45], [~p35, ~p55], [~p35, ~p65], 
  [~p45, ~p55], [~p45, ~p65], 
  [~p55, ~p65], 

  % Chair 6 
  [~p16, ~p26], [~p16, ~p36], [~p16, ~p46], [~p16, ~p56], [~p16, ~p66], 
  [~p26, ~p36], [~p26, ~p46], [~p26, ~p56], [~p26, ~p66],  
  [~p36, ~p46], [~p36, ~p56], [~p36, ~p66], 
  [~p46, ~p56], [~p46, ~p66], 
  [~p56, ~p66], 

  %  1 won't sit next to 2
  [~p11, ~p22], [~p11, ~p26], [~p12, ~p23], [~p12, ~p21],
  [~p13, ~p24], [~p13, ~p22], [~p14, ~p25], [~p14, ~p23],
  [~p15, ~p26], [~p15, ~p24], [~p16, ~p21], [~p16, ~p25], 

  %  3 must  sit next to 4
  [~p31, p42, p46], [~p32, p43, p41], [~p33, p44, p42],
  [~p34, p45, p43], [~p35, p46, p44], [~p36, p41, p45],

  %  4 must  sit next to 5
  [~p41, p52, p56], [~p42, p53, p51], [~p43, p54, p52],
  [~p44, p55, p53], [~p45, p56, p54], [~p46, p51, p55],

  %  5 must  sit next to 6 
  [~p51, p62, p66], [~p52, p63, p61], [~p53, p64, p62],
  [~p54, p65, p63], [~p55, p66, p64], [~p56, p61, p65]
  
], _).
