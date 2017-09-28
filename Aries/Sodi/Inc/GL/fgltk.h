c-------------------------------------------------------------------------
c f90gl Version 1.0 September 1996
c 
c William F. Mitchell
c Applied and Computational Mathematics Division
c National Institute of Standards and Technology
c Gaithersburg, MD 20899
c william.mitchell@nist.gov
c 
c This fortran interface for Mesa software and documentation have been produced
c as part of work done by the U.S. Government, and are not subject to copyright
c in the United States.
c 
c The mention of specific products, trademarks, or brand names in the
c documentation is for purposes of identification only.  Such mention is not to
c be interpreted in any way as an endorsement or certification of such products
c or brands by the National Institute of Standards and Technology.
c All trademarks mentioned herein belong to their respective owners.
c-------------------------------------------------------------------------

      integer*4  TK_RGB
      parameter ( TK_RGB = 0 )
      integer*4  TK_INDEX
      parameter ( TK_INDEX = 1 )
      integer*4  TK_SINGLE
      parameter ( TK_SINGLE = 0 )
      integer*4  TK_DOUBLE
      parameter ( TK_DOUBLE = 2 )
      integer*4  TK_DIRECT
      parameter ( TK_DIRECT = 0 )
      integer*4  TK_INDIRECT
      parameter ( TK_INDIRECT = 4 )
      integer*4  TK_ACCUM
      parameter ( TK_ACCUM = 8 )
      integer*4  TK_ALPHA
      parameter ( TK_ALPHA = 16 )
      integer*4  TK_DEPTH
      parameter ( TK_DEPTH = 32 )
      integer*4  TK_STENCIL
      parameter ( TK_STENCIL = 64 )
      integer*4  TK_OVERLAY
      parameter ( TK_OVERLAY = 128 )
      integer*4  TK_UNDERLAY
      parameter ( TK_UNDERLAY = 256 )
      integer*4  TK_X_DISPLAY
      parameter ( TK_X_DISPLAY = 1 )
      integer*4  TK_X_WINDOW
      parameter ( TK_X_WINDOW = 2 )
      integer*4  TK_LEFTBUTTON
      parameter ( TK_LEFTBUTTON = 1 )
      integer*4  TK_RIGHTBUTTON
      parameter ( TK_RIGHTBUTTON = 2 )
      integer*4  TK_MIDDLEBUTTON
      parameter ( TK_MIDDLEBUTTON = 4 )
      integer*4  TK_SHIFT
      parameter ( TK_SHIFT = 1 )
      integer*4  TK_CONTROL
      parameter ( TK_CONTROL = 2 )
      integer*4  TK_RETURN
      parameter ( TK_RETURN = 13 )
      integer*4  TK_ESCAPE
      parameter ( TK_ESCAPE = 16+11 )
      integer*4  TK_SPACE
      parameter ( TK_SPACE = 2*16 )
      integer*4  TK_LEFT
      parameter ( TK_LEFT = 2*16+5 )
      integer*4  TK_UP
      parameter ( TK_UP = 2*16+6 )
      integer*4  TK_RIGHT
      parameter ( TK_RIGHT = 2*16+7 )
      integer*4  TK_DOWN
      parameter ( TK_DOWN = 2*16+8 )
      integer*4  TK_A
      parameter ( TK_A = ichar('A') )
      integer*4  TK_B
      parameter ( TK_B = ichar('B') )
      integer*4  TK_C
      parameter ( TK_C = ichar('C') )
      integer*4  TK_D
      parameter ( TK_D = ichar('D') )
      integer*4  TK_E
      parameter ( TK_E = ichar('E') )
      integer*4  TK_F
      parameter ( TK_F = ichar('F') )
      integer*4  TK_G
      parameter ( TK_G = ichar('G') )
      integer*4  TK_H
      parameter ( TK_H = ichar('H') )
      integer*4  TK_I
      parameter ( TK_I = ichar('I') )
      integer*4  TK_J
      parameter ( TK_J = ichar('J') )
      integer*4  TK_K
      parameter ( TK_K = ichar('K') )
      integer*4  TK_L
      parameter ( TK_L = ichar('L') )
      integer*4  TK_M
      parameter ( TK_M = ichar('M') )
      integer*4  TK_N
      parameter ( TK_N = ichar('N') )
      integer*4  TK_O
      parameter ( TK_O = ichar('O') )
      integer*4  TK_P
      parameter ( TK_P = ichar('P') )
      integer*4  TK_Q
      parameter ( TK_Q = ichar('Q') )
      integer*4  TK_R
      parameter ( TK_R = ichar('R') )
      integer*4  TK_S
      parameter ( TK_S = ichar('S') )
      integer*4  TK_T
      parameter ( TK_T = ichar('T') )
      integer*4  TK_U
      parameter ( TK_U = ichar('U') )
      integer*4  TK_V
      parameter ( TK_V = ichar('V') )
      integer*4  TK_W
      parameter ( TK_W = ichar('W') )
      integer*4  TK_X
      parameter ( TK_X = ichar('X') )
      integer*4  TK_Y
      parameter ( TK_Y = ichar('Y') )
      integer*4  TK_Z
      parameter ( TK_Z = ichar('Z') )
      integer*4  TK_lc_a
      parameter ( TK_lc_a = ichar('a') )
      integer*4  TK_lc_b
      parameter ( TK_lc_b = ichar('b') )
      integer*4  TK_lc_c
      parameter ( TK_lc_c = ichar('c') )
      integer*4  TK_lc_d
      parameter ( TK_lc_d = ichar('d') )
      integer*4  TK_lc_e
      parameter ( TK_lc_e = ichar('e') )
      integer*4  TK_lc_f
      parameter ( TK_lc_f = ichar('f') )
      integer*4  TK_lc_g
      parameter ( TK_lc_g = ichar('g') )
      integer*4  TK_lc_h
      parameter ( TK_lc_h = ichar('h') )
      integer*4  TK_lc_i
      parameter ( TK_lc_i = ichar('i') )
      integer*4  TK_lc_j
      parameter ( TK_lc_j = ichar('j') )
      integer*4  TK_lc_k
      parameter ( TK_lc_k = ichar('k') )
      integer*4  TK_lc_l
      parameter ( TK_lc_l = ichar('l') )
      integer*4  TK_lc_m
      parameter ( TK_lc_m = ichar('m') )
      integer*4  TK_lc_n
      parameter ( TK_lc_n = ichar('n') )
      integer*4  TK_lc_o
      parameter ( TK_lc_o = ichar('o') )
      integer*4  TK_lc_p
      parameter ( TK_lc_p = ichar('p') )
      integer*4  TK_lc_q
      parameter ( TK_lc_q = ichar('q') )
      integer*4  TK_lc_r
      parameter ( TK_lc_r = ichar('r') )
      integer*4  TK_lc_s
      parameter ( TK_lc_s = ichar('s') )
      integer*4  TK_lc_t
      parameter ( TK_lc_t = ichar('t') )
      integer*4  TK_lc_u
      parameter ( TK_lc_u = ichar('u') )
      integer*4  TK_lc_v
      parameter ( TK_lc_v = ichar('v') )
      integer*4  TK_lc_w
      parameter ( TK_lc_w = ichar('w') )
      integer*4  TK_lc_x
      parameter ( TK_lc_x = ichar('x') )
      integer*4  TK_lc_y
      parameter ( TK_lc_y = ichar('y') )
      integer*4  TK_lc_z
      parameter ( TK_lc_z = ichar('z') )
      integer*4  TK_0
      parameter ( TK_0 = ichar('0') )
      integer*4  TK_1
      parameter ( TK_1 = ichar('1') )
      integer*4  TK_2
      parameter ( TK_2 = ichar('2') )
      integer*4  TK_3
      parameter ( TK_3 = ichar('3') )
      integer*4  TK_4
      parameter ( TK_4 = ichar('4') )
      integer*4  TK_5
      parameter ( TK_5 = ichar('5') )
      integer*4  TK_6
      parameter ( TK_6 = ichar('6') )
      integer*4  TK_7
      parameter ( TK_7 = ichar('7') )
      integer*4  TK_8
      parameter ( TK_8 = ichar('8') )
      integer*4  TK_9
      parameter ( TK_9 = ichar('9') )
      integer*4  TK_BLACK
      parameter ( TK_BLACK = 0 )
      integer*4  TK_RED
      parameter ( TK_RED = 1 )
      integer*4  TK_GREEN
      parameter ( TK_GREEN = 2 )
      integer*4  TK_YELLOW
      parameter ( TK_YELLOW = 3 )
      integer*4  TK_BLUE
      parameter ( TK_BLUE = 4 )
      integer*4  TK_MAGENTA
      parameter ( TK_MAGENTA = 5 )
      integer*4  TK_CYAN
      parameter ( TK_CYAN = 6 )
      integer*4  TK_WHITE
      parameter ( TK_WHITE = 7 )

      integer*4 tkinitwindow
      integer*4 tksetwindowlevel
      integer*4 tkgetcolormapsize
      integer*4 tkrgbimageload
      integer*4 tkcreatestrokefont
      integer*4 tkcreateoutlinefont
      integer*4 tkcreatefilledfont
      integer*4 tkcreatebitmapfont
