$PROB run4 - 2-cmt + WT allometry + SEX effect on CL (tested, not adopted)
; Covariate test from run3: add sex effect on CL
; Result: dOFV < 3.84 (1 df); not significant, model not adopted
; Reference model for comparison: run3
; Busulfan IV, 250 adults, 4 mg/kg Q24h x4 doses (3h infusion)
; Structural : 2-cmt IV (ADVAN3 TRANS4)
; Covariates : WT (allometric), SEX on CL (fractional shift)
; IIV        : CL, V1 (BLOCK 2), V2 (diagonal)
; RUV        : proportional + additive

$INPUT ID TIME AMT RATE DV MDV EVID CMT AGE WT HT SEX ALT AST ALP TBILI ALB SCR CRP WBC

$DATA ../../data/busulfan_adults.csv IGNORE=@ IGNORE=(ID.EQ.0)

$SUBROUTINE ADVAN3 TRANS4

$PK
; SEX: 0 = female (reference), 1 = male
; THETA(7): fractional difference in CL for males vs females
FSEX_CL = 1 + THETA(7) * SEX
TVCL    = THETA(1) * (WT / 70)**0.75 * FSEX_CL
TVV1    = THETA(2) * (WT / 70)
TVQ     = THETA(3) * (WT / 70)**0.75
TVV2    = THETA(4) * (WT / 70)
CL      = TVCL * EXP(ETA(1))
V1      = TVV1 * EXP(ETA(2))
Q       = TVQ
V2      = TVV2 * EXP(ETA(3))
S1      = V1 / 1000

$ERROR
IPRED = A(1) / S1
W     = SQRT((THETA(5) * IPRED)**2 + THETA(6)**2)
IF (W.EQ.0) W = 1
IRES  = DV - IPRED
IWRES = IRES / W
Y     = IPRED + W * EPS(1)

$THETA
  (0,   11)       ; 1. TVCL at 70 kg, female (L/h)
  (0,   50)       ; 2. TVV1 at 70 kg (L)
  (0,    4)       ; 3. TVQ  at 70 kg (L/h)
  (0,    5)       ; 4. TVV2 at 70 kg (L)
  (0,    0.10)    ; 5. prop RUV (fraction)
  (0,   30)       ; 6. add  RUV (ng/mL)
  (-0.5, 0.01, 0.5)  ; 7. SEX effect on CL (fractional, male vs female)

$OMEGA BLOCK(2)
  0.05          ; 1. IIV CL
  0.03  0.04    ; 2. IIV V1

$OMEGA
  0.20          ; 3. IIV V2

$SIGMA
  1 FIX

$ESTIMATION MAXEVAL=9999 METHOD=1 INTERACTION PRINT=5 SIG=3 NOABORT

$TABLE ID TIME DV MDV EVID CMT AMT RATE AGE WT HT SEX
  IPRED IRES IWRES CWRES ETA1 ETA2 ETA3
  ONEHEADER NOPRINT FILE=tab_run4
