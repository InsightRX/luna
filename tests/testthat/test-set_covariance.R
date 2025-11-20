test_that("set_covariance() works on a model with V2 but V is specified", {
  model <- pharmr::read_model_from_string("$SIZES PD=100\n\n$PROBLEM Base linear model with oral input\n\n$INPUT ID DV TIME EVID AMT MDV CMT AGE HEIGHT WEIGHT CREATININE ALBUMIN BILIRUBIN HEMATOCRIT SEX RACE ETHNIC ENC_TIME\n$DATA /tmp/RtmpYiwpu8/file33d74abce82_volume/8fe0097e-fe0b-418d-8cd9-b56fefc23deb/19:40:34_2025_11_18_poppk_run/data.csv IGNORE=@\n\n$SUBROUTINES ADVAN4 TRANS4\n\n$ABBR REPLACE ETA_VP1=ETA(1)\n$ABBR REPLACE ETA_QP1=ETA(2)\n$ABBR REPLACE ETA_V2=ETA(3)\n$ABBR REPLACE ETA_CL=ETA(4)\n$PK\nVP1 = THETA(5)*EXP(ETA_VP1)\nQP1 = THETA(4)*EXP(ETA_QP1)\nTVKA = THETA(1)\nTVCL = THETA(2)\nTVV  = THETA(3)\n\nKA=TVKA\nCL = TVCL*EXP(ETA_CL)\n\nV2 = TVV*EXP(ETA_V2)\nS2 = V2\nQ = QP1\nV3 = VP1\n\n$ERROR\nW = 1\nIPRED = F\nIF (IPRED.EQ.0) THEN\n    IPREDADJ = 2.22500000000000E-16\nELSE\n    IPREDADJ = IPRED\nEND IF\nY = IPRED + EPS(1)*IPREDADJ\n\n$THETA  (0, 0.26389) ; POP_KA\n$THETA  (0, 8.10698)  ; POP_CL\n$THETA  (0, 55.1912)   ; POP_V\n$THETA  (0,5.10817) ; POP_QP1\n$THETA  (0,146.883) ; POP_VP1\n$OMEGA  0.05181 ; IIV_VP1\n$OMEGA  0.0080219 ; IIV_QP1\n$OMEGA BLOCK(2)\n0.356 ; IIV_V2\n0.028  0.223 ; IIV_CL\n$SIGMA  0.015453 ; sigma\n$ESTIMATION METHOD=COND INTER MAXEVAL=2000 PRINT=5 POSTHOC NOABORT\n$COVARIANCE UNCONDITIONAL PRINT=E PRECOND=1\n\n$TABLE\n  ID TIME DV EVID MDV PRED IPRED CWRES NPDE\n  NOAPPEND NOPRINT\n  FILE=sdtab\n$TABLE\n  ID CL QP1 TVKA V2 VP1\n  NOAPPEND NOPRINT\n  FILE=patab\n\n\n")
  expect_message(
    model <- model |>
      set_covariance(list("CL~V" = 0.1)),
    "Found parameter V in model as V2"
  )
  expect_true(inherits(model, "pharmpy.model.external.nonmem.model.Model"))
  om <- pharmr::get_omegas(model)
  expect_equal(
    data.frame(om$to_dataframe()),
    structure(list(
      value = c(0.05181, 0.0080219, 0.356, 0.028, 0.223),
      lower = c(0, 0, 0, -Inf, 0),
      upper = c(Inf, Inf, Inf, Inf,  Inf),
      fix = c(FALSE, FALSE, FALSE, FALSE, FALSE)
      ),
      class = "data.frame",
      row.names = c("IIV_VP1", "IIV_QP1", "IIV_V2", "OMEGA_4_3", "IIV_CL")
    )
  )
})
