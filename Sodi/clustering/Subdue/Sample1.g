% Sample graph:
%
%       gh        ih      ij       jk
%    G------->H<-------I--------J------->K
%    |        ^        |                 |
%    |        |        |                 |
%  ga|      ch|      ie|               kc|
%    |        |        |                 |
%    |  ac    |        |   ae      ac    |
%    A------->C        E<-------A------->C
%    |        |        |        |        |
%    |        |        |        |        |
%  ab|      dc|      ef|      ab|      xx|
%    |        |        |        |        |
%    v  bd    |   fd   |  fb    v  bd    |
%    B--------D<-------F------->B--------D
%    |        |                 |        ^
%    |        |                 |        |
%  bl|      md|               bo|      pd|
%    |        |                 |        |
%    v  lm    |   mn      on    v  op    |
%    L------->M------->N--------O------->P
%
%
% Subdue input format:
%
g dc xx
g bl bo
g L O
% vertices
%
v 1 G
v 2 H
v 3 I
v 4 J
v 5 K
v 6 A
v 7 C
v 8 E
v 9 A
v 10 C
v 11 B
v 12 D
v 13 F
v 14 B
v 15 D
v 16 L
v 17 M
v 18 N
v 19 O
v 20 P
%
% edges
%
d 1 2 gh
d 3 2 ih
u 3 4 ij
d 4 5 jk
u 1 6 ga
d 7 2 ch
u 3 8 ie
u 5 10 kc
d 6 7 ac
d 9 8 ae
d 9 10 ac
d 6 11 ab
u 12 7 dc
u 13 8 ef
d 9 14 ab
u 10 15 xx
u 11 12 bd
d 13 12 fd
d 13 14 fb
u 14 15 bd
d 11 16 bl
u 17 12 md
d 14 19 bo
d 20 15 pd
d 16 17 lm
d 17 18 mn
u 18 19 on
d 19 20 op

