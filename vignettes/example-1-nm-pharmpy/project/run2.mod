$PROB run2 - 2-compartment base model, no covariates, proportional+additive RUV
; Structural refinement from run1: 1-cmt -> 2-cmt
; Busulfan IV, 250 adults, 4 mg/kg Q24h x4 doses (3h infusion)
; Structural : 2-cmt IV (ADVAN3 TRANS4)
; Covariates : none
; IIV        : CL, V1 (correlated, BLOCK 2), V2 (diagonal)
; RUV        : proportional + additive

$DATA ../../data/busulfan_adults.csv IGNORE=@ IGNORE=(ID.EQ.0) COMMA

$INPUT ID TIME AMT RATE DV MDV EVID CMT AGE WT HT SEX ALT AST ALP TBILI ALB SCR CRP WBC

$SUBROUTINE ADVAN3 TRANS4

$PK
TVCL = THETA(1)
TVV1 = THETA(2)
TVQ  = THETA(3)
TVV2 = THETA(4)
CL   = TVCL * EXP(ETA(1))
V1   = TVV1 * EXP(ETA(2))
Q    = TVQ
V2   = TVV2 * EXP(ETA(3))
S1   = V1 / 1000

$ERROR
IPRED = A(1) / S1
W     = SQRT((THETA(5) * IPRED)**2 + THETA(6)**2)
IF (W.EQ.0) W = 1
IRES  = DV - IPRED
IWRES = IRES / W
Y     = IPRED + W * EPS(1)

$THETA
  (0,  11)      ; 1. TVCL (L/h)
  (0,  50)      ; 2. TVV1 (L)
  (0,   4)      ; 3. TVQ  (L/h)
  (0,   5)      ; 4. TVV2 (L)
  (0,   0.10)   ; 5. prop RUV (fraction)
  (0,  30)      ; 6. add  RUV (ng/mL)

$OMEGA BLOCK(2)
  0.06          ; 1. IIV CL
  0.04  0.04    ; 2. IIV V1 (cov CL-V1, var V1)

$OMEGA
  0.20          ; 3. IIV V2

$SIGMA
  1 FIX

$ESTIMATION MAXEVAL=9999 METHOD=1 INTERACTION PRINT=5 SIG=3 NOABORT

$TABLE ID TIME DV MDV EVID CMT AMT RATE AGE WT HT SEX
  IPRED IRES IWRES CWRES ETA1 ETA2 ETA3
  ONEHEADER NOPRINT FILE=tab_run2
