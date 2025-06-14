$PROBLEM Base linear model with iv input

$INPUT ID TIME DV MDV EVID SS II AMT 

$DATA nm_data.csv IGNORE=@
  
$SUBROUTINES ADVAN1 TRANS2

$ABBR REPLACE ETA_CL=ETA(1)
$PK
TVCL = THETA(1)
TVV  = THETA(2)

CL=TVCL*EXP(ETA_CL)
V=TVV

S1 = V

$ERROR
W = 1
IPRED = F
Y = IPRED + W * EPS(1)

$THETA  (0, 15)  ; POP_CL
$THETA  (0, 5)   ; POP_V
$OMEGA  0.3  ; IIV_CL
$SIGMA  0.5 ; RUV_ADD

$EST METHOD=1
$COV UNCOND
