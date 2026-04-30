$PROB run1 - 1-compartment base model, no covariates, proportional RUV
; Busulfan IV, 250 adults, 4 mg/kg Q24h x4 doses (3h infusion)
; Structural : 1-cmt IV (ADVAN1 TRANS2)
; Covariates : none
; IIV        : CL, V (diagonal)
; RUV        : proportional only
;
; Dataset    : busulfan_adults.csv
;   Columns  : ID TIME AMT RATE DV MDV EVID CMT
;              AGE WT HT SEX ALT AST ALP TBILI ALB SCR CRP WBC

$INPUT ID TIME AMT RATE DV MDV EVID CMT AGE WT HT SEX ALT AST ALP TBILI ALB SCR CRP WBC

$DATA ../../data/busulfan_adults.csv IGNORE=@ IGNORE=(ID.EQ.0)

$SUBROUTINE ADVAN1 TRANS2

$PK
TVCL = THETA(1)
TVV  = THETA(2)
CL   = TVCL * EXP(ETA(1))
V    = TVV  * EXP(ETA(2))
S1   = V / 1000

$ERROR
IPRED = A(1) / S1
W     = THETA(3) * IPRED
IF (W.EQ.0) W = 1
IRES  = DV - IPRED
IWRES = IRES / W
Y     = IPRED + W * EPS(1)

$THETA
  (0, 10)       ; 1. TVCL (L/h)
  (0, 55)       ; 2. TVV  (L)
  (0, 0.15)     ; 3. prop RUV (fraction)

$OMEGA
  0.10          ; 1. IIV CL (variance)
  0.10          ; 2. IIV V  (variance)

$SIGMA
  1 FIX

$ESTIMATION MAXEVAL=9999 METHOD=1 INTERACTION PRINT=5 SIG=3 NOABORT

$TABLE ID TIME DV MDV EVID CMT AMT RATE AGE WT HT SEX
  IPRED IRES IWRES CWRES ETA1 ETA2
  ONEHEADER NOPRINT FILE=tab_run1
