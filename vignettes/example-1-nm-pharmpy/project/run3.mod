$PROB run3 - 2-cmt + allometric weight scaling on CL and V (fixed exponents)
; Covariate step from run2: add allometric WT scaling
; Exponents fixed at theory-based values: 0.75 for CL/Q, 1.0 for V1/V2
; Busulfan IV, 250 adults, 4 mg/kg Q24h x4 doses (3h infusion)
; Structural : 2-cmt IV (ADVAN3 TRANS4)
; Covariates : WT (allometric, fixed exponents)
; IIV        : CL, V1 (BLOCK 2), V2 (diagonal)
; RUV        : proportional + additive

$INPUT ID TIME AMT RATE DV MDV EVID CMT AGE WT HT SEX ALT AST ALP TBILI ALB SCR CRP WBC

$DATA ../../data/busulfan_adults.csv IGNORE=@ IGNORE=(ID.EQ.0)

$SUBROUTINE ADVAN3 TRANS4

$PK
TVCL = THETA(1) * (WT / 70)**0.75
TVV1 = THETA(2) * (WT / 70)
TVQ  = THETA(3) * (WT / 70)**0.75
TVV2 = THETA(4) * (WT / 70)
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
  (0,  11)      ; 1. TVCL at 70 kg (L/h)
  (0,  50)      ; 2. TVV1 at 70 kg (L)
  (0,   4)      ; 3. TVQ  at 70 kg (L/h)
  (0,   5)      ; 4. TVV2 at 70 kg (L)
  (0,   0.10)   ; 5. prop RUV (fraction)
  (0,  30)      ; 6. add  RUV (ng/mL)

$OMEGA BLOCK(2)
  0.05          ; 1. IIV CL (reduced vs run2 due to WT)
  0.03  0.04    ; 2. IIV V1

$OMEGA
  0.20          ; 3. IIV V2

$SIGMA
  1 FIX

$ESTIMATION MAXEVAL=9999 METHOD=1 INTERACTION PRINT=5 SIG=3 NOABORT

$TABLE ID TIME DV MDV EVID CMT AMT RATE AGE WT HT SEX
  IPRED IRES IWRES CWRES ETA1 ETA2 ETA3
  ONEHEADER NOPRINT FILE=tab_run3
