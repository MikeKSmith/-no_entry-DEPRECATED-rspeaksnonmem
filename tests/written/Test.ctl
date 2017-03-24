$PROB System Test 1
$INPUT SID SEX AGE RACE HT SMOK HCTZ PROP CON DOSE=AMT WT TIME SECR DV DROP=RATE EVID SS II ID OCC
$DATA   input.tab IGNORE=@
$PRED
KA = THETA(1)
KE = THETA(2)
CL = THETA(3)
F = (DOSE*KE*KA) /(CL * (KE-KA)) * (EXP(-KE*TIME) - EXP(-KA*TIME))
Y = F + ERR(1)
IPRED = F
IWRES = F - DV
$THETA 18.7 FIX
87.3 FIX
2.13 FIX
$OMEGA 0.0231 FIX 
$EST METHOD=0 MAXEVAL=0
