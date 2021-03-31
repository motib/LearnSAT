% Copyright 2012-17 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- use_module('../src/dpll').

%  Four- and eight-queens problem

%  Four-queens problem as given in MLCS, Section 6.4.
%  M. Ben-Ari. Mathematical Logic for Computer Science (Third Edition).
%  Springer, 2012.

queens4 :-
  dpll(
  [
  [p11, p12, p13, p14], 
  [p21, p22, p23, p24],
  [p31, p32, p33, p34],
  [p41, p42, p43, p44],
  
  [~p11, ~p12], [~p11, ~p13], [~p11, ~p14], 
  [~p12, ~p13], [~p12, ~p14], [~p13, ~p14],
  [~p21, ~p22], [~p21, ~p23], [~p21, ~p24],
  [~p22, ~p23], [~p22, ~p24], [~p23, ~p24],
  [~p31, ~p32], [~p31, ~p33], [~p31, ~p34],
  [~p32, ~p33], [~p32, ~p34], [~p33, ~p34],
  [~p41, ~p42], [~p41, ~p43], [~p41, ~p44],
  [~p42, ~p43], [~p42, ~p44], [~p43, ~p44],
  
  [~p11, ~p21], [~p11, ~p31], [~p11, ~p41],
  [~p21, ~p31], [~p21, ~p41], [~p31, ~p41],
  [~p12, ~p22], [~p12, ~p32], [~p12, ~p42],
  [~p22, ~p32], [~p22, ~p42], [~p32, ~p42],
  [~p13, ~p23], [~p13, ~p33], [~p13, ~p43],
  [~p23, ~p33], [~p23, ~p43], [~p33, ~p43],
  [~p14, ~p24], [~p14, ~p34], [~p14, ~p44],
  [~p24, ~p34], [~p24, ~p44], [~p34, ~p44],
  
  [~p11, ~p22], [~p11, ~p33], [~p11, ~p44],
  [~p12, ~p21], [~p12, ~p23], [~p12, ~p34],  
  [~p13, ~p22], [~p13, ~p31], [~p13, ~p24],
  [~p14, ~p23], [~p14, ~p32], [~p14, ~p41],
  [~p21, ~p32], [~p21, ~p43],
  [~p22, ~p31], [~p22, ~p33], [~p22, ~p44],
  [~p23, ~p32], [~p23, ~p41], [~p23, ~p34],
  [~p24, ~p33], [~p24, ~p42],
  [~p31, ~p42],
  [~p32, ~p41], [~p32, ~p43],
  [~p33, ~p42], [~p33, ~p44],
  [~p34, ~p43]
	], _).

queens8 :-
  dpll(
[
[p8,p7,p6,p5,p4,p3,p2,p1],
[~p2,~p1],
[~p3,~p1],
[~p4,~p1],
[~p5,~p1],
[~p6,~p1],
[~p7,~p1],
[~p8,~p1],
[~p3,~p2],
[~p4,~p2],
[~p5,~p2],
[~p6,~p2],
[~p7,~p2],
[~p8,~p2],
[~p4,~p3],
[~p5,~p3],
[~p6,~p3],
[~p7,~p3],
[~p8,~p3],
[~p5,~p4],
[~p6,~p4],
[~p7,~p4],
[~p8,~p4],
[~p6,~p5],
[~p7,~p5],
[~p8,~p5],
[~p7,~p6],
[~p8,~p6],
[~p8,~p7],
[p16,p15,p14,p13,p12,p11,p10,p9],
[~p10,~p9],
[~p11,~p9],
[~p12,~p9],
[~p13,~p9],
[~p14,~p9],
[~p15,~p9],
[~p16,~p9],
[~p11,~p10],
[~p12,~p10],
[~p13,~p10],
[~p14,~p10],
[~p15,~p10],
[~p16,~p10],
[~p12,~p11],
[~p13,~p11],
[~p14,~p11],
[~p15,~p11],
[~p16,~p11],
[~p13,~p12],
[~p14,~p12],
[~p15,~p12],
[~p16,~p12],
[~p14,~p13],
[~p15,~p13],
[~p16,~p13],
[~p15,~p14],
[~p16,~p14],
[~p16,~p15],
[p24,p23,p22,p21,p20,p19,p18,p17],
[~p18,~p17],
[~p19,~p17],
[~p20,~p17],
[~p21,~p17],
[~p22,~p17],
[~p23,~p17],
[~p24,~p17],
[~p19,~p18],
[~p20,~p18],
[~p21,~p18],
[~p22,~p18],
[~p23,~p18],
[~p24,~p18],
[~p20,~p19],
[~p21,~p19],
[~p22,~p19],
[~p23,~p19],
[~p24,~p19],
[~p21,~p20],
[~p22,~p20],
[~p23,~p20],
[~p24,~p20],
[~p22,~p21],
[~p23,~p21],
[~p24,~p21],
[~p23,~p22],
[~p24,~p22],
[~p24,~p23],
[p32,p31,p30,p29,p28,p27,p26,p25],
[~p26,~p25],
[~p27,~p25],
[~p28,~p25],
[~p29,~p25],
[~p30,~p25],
[~p31,~p25],
[~p32,~p25],
[~p27,~p26],
[~p28,~p26],
[~p29,~p26],
[~p30,~p26],
[~p31,~p26],
[~p32,~p26],
[~p28,~p27],
[~p29,~p27],
[~p30,~p27],
[~p31,~p27],
[~p32,~p27],
[~p29,~p28],
[~p30,~p28],
[~p31,~p28],
[~p32,~p28],
[~p30,~p29],
[~p31,~p29],
[~p32,~p29],
[~p31,~p30],
[~p32,~p30],
[~p32,~p31],
[p40,p39,p38,p37,p36,p35,p34,p33],
[~p34,~p33],
[~p35,~p33],
[~p36,~p33],
[~p37,~p33],
[~p38,~p33],
[~p39,~p33],
[~p40,~p33],
[~p35,~p34],
[~p36,~p34],
[~p37,~p34],
[~p38,~p34],
[~p39,~p34],
[~p40,~p34],
[~p36,~p35],
[~p37,~p35],
[~p38,~p35],
[~p39,~p35],
[~p40,~p35],
[~p37,~p36],
[~p38,~p36],
[~p39,~p36],
[~p40,~p36],
[~p38,~p37],
[~p39,~p37],
[~p40,~p37],
[~p39,~p38],
[~p40,~p38],
[~p40,~p39],
[p48,p47,p46,p45,p44,p43,p42,p41],
[~p42,~p41],
[~p43,~p41],
[~p44,~p41],
[~p45,~p41],
[~p46,~p41],
[~p47,~p41],
[~p48,~p41],
[~p43,~p42],
[~p44,~p42],
[~p45,~p42],
[~p46,~p42],
[~p47,~p42],
[~p48,~p42],
[~p44,~p43],
[~p45,~p43],
[~p46,~p43],
[~p47,~p43],
[~p48,~p43],
[~p45,~p44],
[~p46,~p44],
[~p47,~p44],
[~p48,~p44],
[~p46,~p45],
[~p47,~p45],
[~p48,~p45],
[~p47,~p46],
[~p48,~p46],
[~p48,~p47],
[p56,p55,p54,p53,p52,p51,p50,p49],
[~p50,~p49],
[~p51,~p49],
[~p52,~p49],
[~p53,~p49],
[~p54,~p49],
[~p55,~p49],
[~p56,~p49],
[~p51,~p50],
[~p52,~p50],
[~p53,~p50],
[~p54,~p50],
[~p55,~p50],
[~p56,~p50],
[~p52,~p51],
[~p53,~p51],
[~p54,~p51],
[~p55,~p51],
[~p56,~p51],
[~p53,~p52],
[~p54,~p52],
[~p55,~p52],
[~p56,~p52],
[~p54,~p53],
[~p55,~p53],
[~p56,~p53],
[~p55,~p54],
[~p56,~p54],
[~p56,~p55],
[p64,p63,p62,p61,p60,p59,p58,p57],
[~p58,~p57],
[~p59,~p57],
[~p60,~p57],
[~p61,~p57],
[~p62,~p57],
[~p63,~p57],
[~p64,~p57],
[~p59,~p58],
[~p60,~p58],
[~p61,~p58],
[~p62,~p58],
[~p63,~p58],
[~p64,~p58],
[~p60,~p59],
[~p61,~p59],
[~p62,~p59],
[~p63,~p59],
[~p64,~p59],
[~p61,~p60],
[~p62,~p60],
[~p63,~p60],
[~p64,~p60],
[~p62,~p61],
[~p63,~p61],
[~p64,~p61],
[~p63,~p62],
[~p64,~p62],
[~p64,~p63],
[p57,p49,p41,p33,p25,p17,p9,p1],
[~p9,~p1],
[~p17,~p1],
[~p25,~p1],
[~p33,~p1],
[~p41,~p1],
[~p49,~p1],
[~p57,~p1],
[~p17,~p9],
[~p25,~p9],
[~p33,~p9],
[~p41,~p9],
[~p49,~p9],
[~p57,~p9],
[~p25,~p17],
[~p33,~p17],
[~p41,~p17],
[~p49,~p17],
[~p57,~p17],
[~p33,~p25],
[~p41,~p25],
[~p49,~p25],
[~p57,~p25],
[~p41,~p33],
[~p49,~p33],
[~p57,~p33],
[~p49,~p41],
[~p57,~p41],
[~p57,~p49],
[p58,p50,p42,p34,p26,p18,p10,p2],
[~p10,~p2],
[~p18,~p2],
[~p26,~p2],
[~p34,~p2],
[~p42,~p2],
[~p50,~p2],
[~p58,~p2],
[~p18,~p10],
[~p26,~p10],
[~p34,~p10],
[~p42,~p10],
[~p50,~p10],
[~p58,~p10],
[~p26,~p18],
[~p34,~p18],
[~p42,~p18],
[~p50,~p18],
[~p58,~p18],
[~p34,~p26],
[~p42,~p26],
[~p50,~p26],
[~p58,~p26],
[~p42,~p34],
[~p50,~p34],
[~p58,~p34],
[~p50,~p42],
[~p58,~p42],
[~p58,~p50],
[p59,p51,p43,p35,p27,p19,p11,p3],
[~p11,~p3],
[~p19,~p3],
[~p27,~p3],
[~p35,~p3],
[~p43,~p3],
[~p51,~p3],
[~p59,~p3],
[~p19,~p11],
[~p27,~p11],
[~p35,~p11],
[~p43,~p11],
[~p51,~p11],
[~p59,~p11],
[~p27,~p19],
[~p35,~p19],
[~p43,~p19],
[~p51,~p19],
[~p59,~p19],
[~p35,~p27],
[~p43,~p27],
[~p51,~p27],
[~p59,~p27],
[~p43,~p35],
[~p51,~p35],
[~p59,~p35],
[~p51,~p43],
[~p59,~p43],
[~p59,~p51],
[p60,p52,p44,p36,p28,p20,p12,p4],
[~p12,~p4],
[~p20,~p4],
[~p28,~p4],
[~p36,~p4],
[~p44,~p4],
[~p52,~p4],
[~p60,~p4],
[~p20,~p12],
[~p28,~p12],
[~p36,~p12],
[~p44,~p12],
[~p52,~p12],
[~p60,~p12],
[~p28,~p20],
[~p36,~p20],
[~p44,~p20],
[~p52,~p20],
[~p60,~p20],
[~p36,~p28],
[~p44,~p28],
[~p52,~p28],
[~p60,~p28],
[~p44,~p36],
[~p52,~p36],
[~p60,~p36],
[~p52,~p44],
[~p60,~p44],
[~p60,~p52],
[p61,p53,p45,p37,p29,p21,p13,p5],
[~p13,~p5],
[~p21,~p5],
[~p29,~p5],
[~p37,~p5],
[~p45,~p5],
[~p53,~p5],
[~p61,~p5],
[~p21,~p13],
[~p29,~p13],
[~p37,~p13],
[~p45,~p13],
[~p53,~p13],
[~p61,~p13],
[~p29,~p21],
[~p37,~p21],
[~p45,~p21],
[~p53,~p21],
[~p61,~p21],
[~p37,~p29],
[~p45,~p29],
[~p53,~p29],
[~p61,~p29],
[~p45,~p37],
[~p53,~p37],
[~p61,~p37],
[~p53,~p45],
[~p61,~p45],
[~p61,~p53],
[p62,p54,p46,p38,p30,p22,p14,p6],
[~p14,~p6],
[~p22,~p6],
[~p30,~p6],
[~p38,~p6],
[~p46,~p6],
[~p54,~p6],
[~p62,~p6],
[~p22,~p14],
[~p30,~p14],
[~p38,~p14],
[~p46,~p14],
[~p54,~p14],
[~p62,~p14],
[~p30,~p22],
[~p38,~p22],
[~p46,~p22],
[~p54,~p22],
[~p62,~p22],
[~p38,~p30],
[~p46,~p30],
[~p54,~p30],
[~p62,~p30],
[~p46,~p38],
[~p54,~p38],
[~p62,~p38],
[~p54,~p46],
[~p62,~p46],
[~p62,~p54],
[p63,p55,p47,p39,p31,p23,p15,p7],
[~p15,~p7],
[~p23,~p7],
[~p31,~p7],
[~p39,~p7],
[~p47,~p7],
[~p55,~p7],
[~p63,~p7],
[~p23,~p15],
[~p31,~p15],
[~p39,~p15],
[~p47,~p15],
[~p55,~p15],
[~p63,~p15],
[~p31,~p23],
[~p39,~p23],
[~p47,~p23],
[~p55,~p23],
[~p63,~p23],
[~p39,~p31],
[~p47,~p31],
[~p55,~p31],
[~p63,~p31],
[~p47,~p39],
[~p55,~p39],
[~p63,~p39],
[~p55,~p47],
[~p63,~p47],
[~p63,~p55],
[p64,p56,p48,p40,p32,p24,p16,p8],
[~p16,~p8],
[~p24,~p8],
[~p32,~p8],
[~p40,~p8],
[~p48,~p8],
[~p56,~p8],
[~p64,~p8],
[~p24,~p16],
[~p32,~p16],
[~p40,~p16],
[~p48,~p16],
[~p56,~p16],
[~p64,~p16],
[~p32,~p24],
[~p40,~p24],
[~p48,~p24],
[~p56,~p24],
[~p64,~p24],
[~p40,~p32],
[~p48,~p32],
[~p56,~p32],
[~p64,~p32],
[~p48,~p40],
[~p56,~p40],
[~p64,~p40],
[~p56,~p48],
[~p64,~p48],
[~p64,~p56],
[~p15,~p8],
[~p22,~p8],
[~p29,~p8],
[~p36,~p8],
[~p43,~p8],
[~p50,~p8],
[~p57,~p8],
[~p22,~p15],
[~p29,~p15],
[~p36,~p15],
[~p43,~p15],
[~p50,~p15],
[~p57,~p15],
[~p29,~p22],
[~p36,~p22],
[~p43,~p22],
[~p50,~p22],
[~p57,~p22],
[~p36,~p29],
[~p43,~p29],
[~p50,~p29],
[~p57,~p29],
[~p43,~p36],
[~p50,~p36],
[~p57,~p36],
[~p50,~p43],
[~p57,~p43],
[~p57,~p50],
[~p14,~p7],
[~p21,~p7],
[~p28,~p7],
[~p35,~p7],
[~p42,~p7],
[~p49,~p7],
[~p21,~p14],
[~p28,~p14],
[~p35,~p14],
[~p42,~p14],
[~p49,~p14],
[~p28,~p21],
[~p35,~p21],
[~p42,~p21],
[~p49,~p21],
[~p35,~p28],
[~p42,~p28],
[~p49,~p28],
[~p42,~p35],
[~p49,~p35],
[~p49,~p42],
[~p13,~p6],
[~p20,~p6],
[~p27,~p6],
[~p34,~p6],
[~p41,~p6],
[~p20,~p13],
[~p27,~p13],
[~p34,~p13],
[~p41,~p13],
[~p27,~p20],
[~p34,~p20],
[~p41,~p20],
[~p34,~p27],
[~p41,~p27],
[~p41,~p34],
[~p12,~p5],
[~p19,~p5],
[~p26,~p5],
[~p33,~p5],
[~p19,~p12],
[~p26,~p12],
[~p33,~p12],
[~p26,~p19],
[~p33,~p19],
[~p33,~p26],
[~p11,~p4],
[~p18,~p4],
[~p25,~p4],
[~p18,~p11],
[~p25,~p11],
[~p25,~p18],
[~p10,~p3],
[~p17,~p3],
[~p17,~p10],
[~p9,~p2],
[~p23,~p16],
[~p30,~p16],
[~p37,~p16],
[~p44,~p16],
[~p51,~p16],
[~p58,~p16],
[~p30,~p23],
[~p37,~p23],
[~p44,~p23],
[~p51,~p23],
[~p58,~p23],
[~p37,~p30],
[~p44,~p30],
[~p51,~p30],
[~p58,~p30],
[~p44,~p37],
[~p51,~p37],
[~p58,~p37],
[~p51,~p44],
[~p58,~p44],
[~p58,~p51],
[~p31,~p24],
[~p38,~p24],
[~p45,~p24],
[~p52,~p24],
[~p59,~p24],
[~p38,~p31],
[~p45,~p31],
[~p52,~p31],
[~p59,~p31],
[~p45,~p38],
[~p52,~p38],
[~p59,~p38],
[~p52,~p45],
[~p59,~p45],
[~p59,~p52],
[~p39,~p32],
[~p46,~p32],
[~p53,~p32],
[~p60,~p32],
[~p46,~p39],
[~p53,~p39],
[~p60,~p39],
[~p53,~p46],
[~p60,~p46],
[~p60,~p53],
[~p47,~p40],
[~p54,~p40],
[~p61,~p40],
[~p54,~p47],
[~p61,~p47],
[~p61,~p54],
[~p55,~p48],
[~p62,~p48],
[~p62,~p55],
[~p63,~p56],
[~p16,~p7],
[~p15,~p6],
[~p24,~p6],
[~p24,~p15],
[~p14,~p5],
[~p23,~p5],
[~p32,~p5],
[~p23,~p14],
[~p32,~p14],
[~p32,~p23],
[~p13,~p4],
[~p22,~p4],
[~p31,~p4],
[~p40,~p4],
[~p22,~p13],
[~p31,~p13],
[~p40,~p13],
[~p31,~p22],
[~p40,~p22],
[~p40,~p31],
[~p12,~p3],
[~p21,~p3],
[~p30,~p3],
[~p39,~p3],
[~p48,~p3],
[~p21,~p12],
[~p30,~p12],
[~p39,~p12],
[~p48,~p12],
[~p30,~p21],
[~p39,~p21],
[~p48,~p21],
[~p39,~p30],
[~p48,~p30],
[~p48,~p39],
[~p11,~p2],
[~p20,~p2],
[~p29,~p2],
[~p38,~p2],
[~p47,~p2],
[~p56,~p2],
[~p20,~p11],
[~p29,~p11],
[~p38,~p11],
[~p47,~p11],
[~p56,~p11],
[~p29,~p20],
[~p38,~p20],
[~p47,~p20],
[~p56,~p20],
[~p38,~p29],
[~p47,~p29],
[~p56,~p29],
[~p47,~p38],
[~p56,~p38],
[~p56,~p47],
[~p10,~p1],
[~p19,~p1],
[~p28,~p1],
[~p37,~p1],
[~p46,~p1],
[~p55,~p1],
[~p64,~p1],
[~p19,~p10],
[~p28,~p10],
[~p37,~p10],
[~p46,~p10],
[~p55,~p10],
[~p64,~p10],
[~p28,~p19],
[~p37,~p19],
[~p46,~p19],
[~p55,~p19],
[~p64,~p19],
[~p37,~p28],
[~p46,~p28],
[~p55,~p28],
[~p64,~p28],
[~p46,~p37],
[~p55,~p37],
[~p64,~p37],
[~p55,~p46],
[~p64,~p46],
[~p64,~p55],
[~p18,~p9],
[~p27,~p9],
[~p36,~p9],
[~p45,~p9],
[~p54,~p9],
[~p63,~p9],
[~p27,~p18],
[~p36,~p18],
[~p45,~p18],
[~p54,~p18],
[~p63,~p18],
[~p36,~p27],
[~p45,~p27],
[~p54,~p27],
[~p63,~p27],
[~p45,~p36],
[~p54,~p36],
[~p63,~p36],
[~p54,~p45],
[~p63,~p45],
[~p63,~p54],
[~p26,~p17],
[~p35,~p17],
[~p44,~p17],
[~p53,~p17],
[~p62,~p17],
[~p35,~p26],
[~p44,~p26],
[~p53,~p26],
[~p62,~p26],
[~p44,~p35],
[~p53,~p35],
[~p62,~p35],
[~p53,~p44],
[~p62,~p44],
[~p62,~p53],
[~p34,~p25],
[~p43,~p25],
[~p52,~p25],
[~p61,~p25],
[~p43,~p34],
[~p52,~p34],
[~p61,~p34],
[~p52,~p43],
[~p61,~p43],
[~p61,~p52],
[~p42,~p33],
[~p51,~p33],
[~p60,~p33],
[~p51,~p42],
[~p60,~p42],
[~p60,~p51],
[~p50,~p41],
[~p59,~p41],
[~p59,~p50],
[~p58,~p49]
], _).