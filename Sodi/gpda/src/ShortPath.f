      SUBROUTINE SPATH(J0, Ke, Nnodes, ICols, IRows, W)
      INTEGER J0, Ke, Nnodes, ICols(1), IRows(1), W(1)
C
C   Program Arguments:
C   ------------------
C
C       J0        In     Starting node (1..35)
C       Ke        In     Ending node (2..36)
C       Nnodes    Out    No. nodes in path
C       ICols     Out    List of column #s of nodes in path
C       IRows     Out    List of row #s of nodes in path
C       W         Out    List of node #s in path
C
C   Program Description:
C   --------------------
C
C   THIS PROGRAM CALCULATES THE SHORTEST PATH LENGTHS FROM A SPECIFIC   PAP00004
C   NODE (J0) TO ALL OTHER NODES IN A NETWORK  AND THE SHORTEST PATH    PAP00005
C   BETWEEN THIS NODE (J0) AND ANOTHER NODE (KE).                       PAP00006
C                                                                       PAP00007
C   Local Variables:
C   ----------------
C
C     FLIST(NUMBER OF EDGES)                                            PAP00008
C     DFLIST(NUMBER OF EDGES)                                           PAP00009
C     KF(NUMBER OF NODES+1)                                             PAP00010
C                                                                       PAP00011
C     FLIST, DFLIST, AND KF REPRESENT THE NETWORK                       PAP00012
C                                                                       PAP00013
C     MJ(NUMBER OF NODES)                                               PAP00014
C     WJ(NUMBER OF NODES)                                               PAP00015
C     W(NUMBER OF NODES)                                                PAP00016
C     H(NUMBER OF NODES)                                                PAP00017
C     INPUT                  CHANNEL FOR INPUT NETWORK                  PAP00022
C     OUTP                   CHANNEL FOR OUTPUT                         PAP00023
C
C --------------------------------------------------------------------
C
      LOGICAL LDEBUG
      INTEGER FLIST(200), DFLIST(200), KF(51), MJ(50), WJ(50), H(50),   PAP00001
     +        INPUT, OUTP, TEMP(10), INODE                              PAP00002
      CHARACTER*1 CH
C                                                                       PAP00003
      DATA INF, INPUT, OUTP, LDEBUG / 1000000, 10, 6, .FALSE. /         PAP00019
C                                                                       PAP00020
100   FORMAT(10I8)                                                      PAP00069
101   FORMAT(I4)                                                        PAP00070
200   FORMAT(21H DISTANCES FROM NODE ,I5/)                              PAP00071
201   FORMAT(10(I7,1H-,I4))                                             PAP00072
202   FORMAT(//15H PATH FROM NODE,I7,9H TO NODE ,I7,13H (PATHLENGTH=,   PAP00073
     +       I7,1H))                                                    PAP00074
c203   FORMAT(1H ,20I6)                                                 PAP00075
203   FORMAT(1H , I6, 1H(, I3, 1H,, I3, 1H) )
204   FORMAT(1A1)
C
C --------------------------------------------------------------------  PAP00026
C                                                                       PAP00030
C       Input the Network                                               PAP00031
C
      OPEN(INPUT, FILE="TempPath.out")
C
    1 READ(INPUT,100) N                                                 PAP00027
      READ(INPUT,204) CH
      K=0                                                               PAP00028
      KF(1)=0                                                           PAP00029
C                                                                       PAP00032
      DO 4 I=1,N                                                        PAP00033
        READ(INPUT,100) INODE, ICOL, IROW, IT,(TEMP(J),J=1,IT)          PAP00034
        ICOLS(I) = ICOL
        IROWS(I) = IROW
        IF (IT.EQ.0) GO TO 3                                            PAP00035
        JA=K+1                                                          PAP00036
        JB=K+IT                                                         PAP00037
        JJ=1                                                            PAP00038
        DO 2 J=JA,JB                                                    PAP00039
          FLIST(J)=TEMP(JJ)                                             PAP00040
    2   JJ=JJ+1                                                         PAP00041
C                                                                       PAP00042
        READ(INPUT,100) IDUMMY,(DFLIST(J),J=JA,JB)                      PAP00043
        K=K+IT                                                          PAP00044
    3   KF(I+1)=K                                                       PAP00045
    4 CONTINUE                                                          PAP00046
C                                                                       PAP00052
C       Find distance to all nodes below this one
C
      CALL SHPTHL(FLIST, DFLIST, KF, N, MJ, J0, WJ)                     PAP00053
C                                                                       PAP00054
      IF (LDEBUG) THEN
        WRITE(OUTP,200) J0                                              PAP00055
        IF (N.GT.0) WRITE(OUTP,201) (I,MJ(I),I=1,N)                     PAP00056
      ENDIF
C                                                                       PAP00062
C       Find shortest path from 'J0' to 'Ke'
C
      CALL SHPATH(J0, Ke, WJ, H, NI, W)                                 PAP00063
C                                                                       PAP00064
      IF (LDEBUG) THEN
        WRITE(OUTP,202) J0, KE, MJ(KE)                                  PAP00065
        DO 20 I=1,NI
          WRITE(OUTP,203) W(I), ICOLS(W(I)), IROWS(W(I))                PAP00066
 20     CONTINUE
      ENDIF
c
      Nnodes = NI
C                                                                       PAP00068
C        Finish up and exit
C
 9999 CONTINUE
      CLOSE(INPUT)
      RETURN
C
      END                                                               PAP00076

      SUBROUTINE SHPTHL(FLIST,DFLIST,KF,N,MJ,J0,WJ)                     SPL00001
      INTEGER FLIST(200),DFLIST(200),KF(51),MJ(50),NJ(50),WJ(50)
      DATA INF /1000000/
C
C     SHPTHL CALCULATES THE SHORTEST PATH LENGTHS (MJ) FROM A SPECIFIC
C     NODE (J0) TO ALL OTHER (N-1) NODES IN A NETWORK (FLIST,DFLIST,KF).
C     PREDECESSOR NODES ARE STORED IN WJ.
C
C     FLIST : FORWARD INDEX LIST
C     DFLIST: DISTANCE LIST
C     KF    : POINTER LIST FOR FLIST AND DFLIST
C     N     : NUMBER OF NODES
C     MJ    : ARRAY OF SHORTEST PATH LENGTHS
C     JO    : INITIAL NODE, FIRST NODE OF SHORTEST PATH
C     WJ    : ARRAY OF PREDECESSORS FOR SHORTEST PATH CONSTRUCTION
C     NJ    : DOUBLE ENDED QUEUE FOR NODE DISCUSSION
C     INF   : A LARGE NUMBER
C
C     INITIAL VALUES OF
C     FLIST,DFLIST,KF: NETWORK FROM INPUT AND MAIN PROGRAM
C     N              : NUMBER OF NODES FROM INPUT AND MAIN PROGRAM
C     J0             : START NODE FROM INPUT AND MAIN PROGRAM
C
      DO 1 I=1,N
      MJ(I)=INF
    1 NJ(I)=0
      MJ(J0)=0
C
C     I     : INDEX FOR NODE DISCUSSION, NODE UNDER DISCUSSION
C     NT    : POINTER TO THE END OF DEQUE NJ
C     MJI   : LOCAL VARIABLE OF MJ(I)
C     KFI   : LOCAL VARIABLE OF KF(I)
C     KFI1  : LOCAL VARIABLE OF KF(I)+1
C     IR    : INDEX FOR ARRAY DISCUSSION
C     K     : SUCCESSOR OF NODE I
C     MJK   : LOCAL VARIABLE OF MJ(K)
C     NJI   : LOCAL VARIABLE OF NJ(I), THE NEXT NODE OF NJ TO BE TAKEN
C             UNDER DISCUSSION
C
      NJ(J0)=INF
      I=J0
      NT=J0
C
C     OUTER LOOP
C     DISCUSSION OF NODES I
C
    2 KFI=KF(I+1)
      MJI=MJ(I)
      KFI1=KF(I)+1
C
C     INNER  LOOP
C     DISCUSSION OF SUCCESSORS K
C
      IF (KFI1.GT.KFI) GO TO 6
      DO 5 IR=KFI1,KFI
      K=FLIST(IR)
      MJK=MJI+DFLIST(IR)
C     NO DECREASE OF SHORTEST DISTANCES
      IF (MJK.GE.MJ(K)) GO TO 5
C     DECREASE OF SHORTEST DISTANCES
      MJ(K)=MJK
C     PREDECESSOR I OF NODE K
      WJ(K)=I
C     NODE K ALREADY IN THE DEQUE NJ ?
      IF (NJ(K))  4,3,5
C     NODE K ADDED AT THE END OF THE DEQUE NJ
    3 NJ(NT)=K
      NT=K
      NJ(K)=INF
      GO TO 5
C     NODE K ADDED AT THE BEGINNING OF THE DEQUE NJ
    4 NJ(K)=NJ(I)
      NJ(I)=K
      IF (NT.EQ.I) NT = K
    5 CONTINUE
C     NODE I TAKEN FROM THE BEGINNING OF THE DEQUE NJ
    6 NJI=NJ(I)
      NJ(I)=-NJI
      I=NJI
      IF (I.LT.INF) GOTO 2
      RETURN
      END

      SUBROUTINE SHPATH(J0,KE,WJ,H,NI,W)                                PAT00001
      INTEGER WJ(50),H(50),W(50)
C
C     SHPATH CALCULATES THE SHORTEST PATH BETWEEN THE TWO NODES J0 AND
C     KE. SHPATH USES THE INFORMATION IN WJ, THE LIST OF PREDECESSOR
C     NODES. THE NODES OF THE SHORTEST PATH ARE STORED IN W.
C
C     J0    : FIRST NODE OF THE SHORTEST PATH
C     KE    : LAST NODE OF THE SHORTEST PATH
C     WJ    : ARRAY OF PREDECESSORS FOR SHORTEST PATH CONSTRUCTION
C     H     : AUXILIARY ARRAY FOR THE SHORTEST PATH
C     NI    : NUMBER OF NODES OF THE SHORTEST PATH
C     W     : THE SHORTEST PATH
C
C     I,J   : LOCAL VARIABLE FOR NODE DISCUSSION
C
C     INITIAL VALUES OF
C     J0    : FIRST NODE FROM INPUT OR MAIN PROGRAM
C     KE    : LAST NODE FROM INPUT OR MAIN PROGRAM
C     WJ    : FROM SUBROUTINE SHPTHL
C
      H(1)=KE
      I=1
      J=KE
      IF (J0.EQ.KE) GO TO 2
C
    1 I=I+1
      J=WJ(J)
      H(I)=J
      IF (J.NE.J0) GO TO 1
C
    2 NI=I
C
      DO 3 I=1,NI
    3 W(I)=H(NI+1-I)
C
      RETURN
      END
