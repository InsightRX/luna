$PROBLEM Base linear model with oral input

$INPUT ID TIME DV MDV EVID SS II AMT 

$DATA nm_data.csv IGNORE=@

$SUBROUTINES ADVAN2 TRANS2

$ABBR REPLACE ETA_CL=ETA(1)
$PK
TVKA = THETA(1)
TVCL = THETA(2)
TVV  = THETA(3)

KA=TVKA
CL=TVCL*EXP(ETA_CL)
V=TVV

S2 = V

$ERROR
W = 1
IPRED = F
Y = IPRED + W * EPS(1)

$THETA  (0, 0.5) ; POP_KA
$THETA  (0, 15)  ; POP_CL
$THETA  (0, 5)   ; POP_V
$OMEGA  0.3  ; IIV_CL
$SIGMA  0.5 ; RUV_ADD

$EST METHOD=1
$COV UNCOND
