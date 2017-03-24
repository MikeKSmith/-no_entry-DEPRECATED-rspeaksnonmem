$PROBLEM run1.mod run1.ctl MOXONIDINE PK ANALYSIS
$INPUT      ID PAT=DROP VISI XAT2=DROP DGRP=DROP DOSE=DROP FLAG=DROP ONO=DROP
            XIME=DROP DVO=DROP NEUY=DROP SCR=DROP AGE SEX NYHA=DROP WT
            COMP ACE DIG DIU NUMB=DROP TAD TIME VIDD=DROP CLCR AMT SS
            II VID CMT=DROP CONO=DROP DV EVID=DROP OVID=DROP RGRP
$DATA mx2007.csv IGNORE=@
$SUBROUTINE ADVAN2 TRANS1
$PK

;-----------OCCASIONS----------
   VIS3               = 0
   IF(VISI.EQ.3) VIS3 = 1
   VIS8               = 0
   IF(VISI.EQ.8) VIS8 = 1

;----------IOV--------------------
   
   KPCL  = VIS3*ETA(4)+VIS8*ETA(5)
   KPKA  = VIS3*ETA(6)+VIS8*ETA(7)

;---------- PK model ------------------

   TVCL  = THETA(1)*(1+THETA(7)*(CLCR-65))
   TVV   = THETA(2)*WT
   TVKA  = THETA(3)

   CL    = TVCL*EXP(ETA(1)+KPCL)
   V     = TVV*EXP(ETA(2))  
   KA    = TVKA*EXP(ETA(3)+KPKA)
   ALAG1 = THETA(4)
   K     = CL/V
   S2    = V

$ERROR

     IPRED = LOG(.025)
     IF(F.GT.0) IPRED = LOG(F)
     W     = (THETA(6) + THETA(5)/IPRED) ;* EXP(ETA(8))
     IRES  = IPRED-DV
     IWRES = IRES/W
     Y     = IPRED+EPS(1)*W

$THETA  (0,27.1)              ;TVCL        ; 1
$THETA  (0,1.49)              ;TVV         ; 2 ;This is in L per kg WT
$THETA  (0,3.52)              ;TVKA        ; 3
$THETA  (0,.227)              ;LAG         ; 4
$THETA  0 FIX                 ;Add.err     ; 5
$THETA  (0,.343)              ;Prop.err    ; 6
$THETA  (0,.00646,.02941)     ;CRCL.on.CL  ; 7

$OMEGA  BLOCK(2) 0.0489       ;IIV.CL      ; 1
                 0.0256       ;COV.ETA.CL.V; 1-2
                 0.0156       ;IIV.V       ; 2
$OMEGA  BLOCK(1) 1.98         ;IIV.KA      ; 3
$OMEGA  BLOCK(1) 0.013        ;IOV.CL      ; 4
$OMEGA  BLOCK(1) SAME         ;IOV.CL.2    ; 5 
$OMEGA  BLOCK(1) 0.0481       ;IOV.KA      ; 6
$OMEGA  BLOCK(1) SAME         ;IOV.KA.2    ; 7
;$OMEGA          0.001        ;IIV.EPS     ; 8

$SIGMA  1  FIX                ;ResFixed    ; 1
$ESTIMATION METHOD=1 MAXEVALS=9999 PRINT=1 MSFO=msf1
;$COVARIANCE PRINT=E

$TAB ID TAD IPRED IWRES CWRES               ONEHEADER NOPRINT FILE = sdtab1
$TAB ID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 ETA7  ONEHEADER NOPRINT FILE = patab1 NOAPPEND
$TAB ID AGE WT CLCR                         ONEHEADER NOPRINT FILE = cotab1 NOAPPEND
$TAB ID SEX ACE DIG DIU COMP VISI           ONEHEADER NOPRINT FILE = catab1 NOAPPEND



