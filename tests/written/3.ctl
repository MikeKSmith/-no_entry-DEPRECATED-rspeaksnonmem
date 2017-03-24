$PROB 3 phase1 2 CMT like 1004 but diff. initial on V3
$INPUT C ID TIME SEQ=DROP EVID AMT DV SUBJ HOUR HEIGHT WT SEX AGE DOSE FED
$DATA   ../../data/derived/phase1.csv IGNORE=C
$SUBROUTINE ADVAN4 TRANS4
$PK
CL=THETA(1)*EXP(ETA(1)) * THETA(6)**SEX * (WT/70)**THETA(7)
V2 =THETA(2)*EXP(ETA(2))
KA=THETA(3)*EXP(ETA(3))
Q  =THETA(4)
V3=THETA(5)
S2=V2
$ERROR
Y=F*(1+ERR(1)) + ERR(2)
IPRE=F
$THETA 8.257
21.93
0.06598
3.722
74.43
0.8294
2.14
$OMEGA 0.1647   
0.1277   
0.1130  
$SIGMA 0.06041  
$SIMULATION (7998 NEW) (8999 UNIFORM)  ONLYSIMULATION 
$TABLE ID TIME DV WT SEX LDOS NOAPPEND ONEHEADER NOPRINT FILE=sim.tab
