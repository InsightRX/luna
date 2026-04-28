# run1 - 1-compartment base model, no covariates, proportional RUV
# Busulfan IV, 250 adults, 4 mg/kg Q24h x4 doses (3h infusion)
# AMT in mg, V in L → linCmt() in mg/L × 1000 = ng/mL

run1_model <- function() {
  ini({
    tvcl     <- 10        # TVCL (L/h)
    tvv      <- 55        # TVV  (L)
    eta.cl   ~ 0.10       # IIV CL (variance)
    eta.v    ~ 0.10       # IIV V  (variance)
    prop.err <- 0.15      # proportional RUV (fraction)
  })
  model({
    cl <- tvcl * exp(eta.cl)
    v  <- tvv  * exp(eta.v)
    cp <- linCmt() * 1000   # ng/mL
    cp ~ prop(prop.err)
  })
}
