$PROB 1006 like 1005 with tweaked initial estimates
$INPUT C ID TIME SEQ=DROP EVID AMT DV SUBJ HOUR HEIGHT WT SEX AGE DOSE FED
$DATA ../../data/derived/phase1.csv IGNORE=C
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
;<doc>
$THETA
; 
(0,11.6,50)
(0,9.58,100)
(0,0.235,5)
(0,11.7,50)
(0,105,1000)
(0,0.8,2)
(0,0.659,3)
$OMEGA BLOCK(3)
.1
.01 .1
.01 .01 .1

;<parameter name='OMEGA1.1' label='$\Omega^{1.1}CL/F$'>interindividual variability on clearance</parameter>
;<parameter name='OMEGA2.1' label='$\Omega^{2.1}Vc/F$'>interindividual clearance-volume covariance</parameter>
;<parameter name='OMEGA2.2' label='$\Omega^{2.2}Vc/F$'>interindividual variability on central volume</parameter>
;<parameter name='OMEGA3.1' label='$\Omega^{3.1}Ka$'>interindividual clearance-Ka covariance</parameter>
;<parameter name='OMEGA3.2' label='$\Omega^{3.2}Ka$'>interindividual volume-Ka covariance</parameter>
;<parameter name='OMEGA3.3' label='$\Omega^{3.3}Ka$'>interindividual variability on Ka</parameter>

$SIGMA 0.1 0.1

;<parameter name='SIGMA1.1' label='$\sigma^{1.1}prop$'>proportional error</parameter>
;<parameter name='SIGMA2.2' label='$\sigma^{2.2}prop$'>additive error</parameter>

$ESTIMATION MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTER MSFO=./1006.msf
$COV PRINT=E
$TABLE NOPRINT FILE=./1006.tab ONEHEADER ID AMT TIME EVID PRED IPRE CWRES
NPRED
CPRED
CPREDI
EPRED
RES
NRES
NWRES
CRES
RESI
WRESI
CRESI
CWRESI
CIWRES
CIWRESI
ERES
EWRES
ECWRES
$TABLE NOPRINT FILE=./1006par.tab ONEHEADER ID TIME CL Q V2 V3 KA ETA1 ETA2 ETA3

;<wiki model='CL/F (L/h) ~ theta_1 * theta_6 ^MALE * (WT/70)^theta_7  * e^eta_1'>apparent oral clearance</wiki>
;<wiki model='V_c /F (L) ~ theta_2 * (WT/70)^1 * e^eta_2'                       >central volume of distribution</wiki>
;<wiki model='K_a (h^-1 ) ~ theta_3 * e^eta_3'                                  >absorption rate constant</wiki>
;<wiki model='Q/F (L/h) ~ theta_4'                                              >intercompartmental clearance</wiki>
;<wiki model='V_p /F (L) ~ theta_5'                                             >peripheral volume of distribution</wiki>
;<wiki model='MALE_CL/F ~ theta_6'                                              >male effect on clearance</wiki>
;<wiki model = 'WT_CL/F ~ theta_7'                                              >weight effect on clearance</wiki>
;<wiki model='IIV_CL/F ~ Omega_1.1'                                             >interindividual variability of clearance</wiki>
;<wiki model='cov_CL,V ~ Omega_2.1'                                             >interindividual clearance-volume covariance</wiki>
;<wiki model='IIV_V_c /F ~ Omega_2.2'                                           >interindividual variability of central volume</wiki>
;<wiki model='cov_CL,Ka  ~ Omega_3.1'                                           >interindividual clearance-Ka covariance</wiki>
;<wiki model='cov_V,Ka  ~ Omega_3.2'                                            >interindividual volume-Ka covariance</wiki>
;<wiki model='IIV_K_a  ~ Omega_3.3'                                             >interindividual variability of Ka</wiki>
;<wiki model='err_prop ~ Sigma_1.1'                                             >proportional error</wiki>
;<wiki model='err_add ~ Sigma_2.2'                                              >additive error</wiki>
;</doc>
