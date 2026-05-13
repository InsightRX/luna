# run3 - 2-cmt + allometric WT scaling on CL and V (fixed exponents)
# Covariate step from run2: add allometric WT scaling
# Exponents fixed at theory-based values: 0.75 for flow parameters, 1.0 for volumes
# Reference parameter values at 70 kg

run3_model <- function() {
  ini({
    tvcl     <- c(0, 11)   # TVCL at 70 kg (L/h)
    tvv      <- c(0, 50)   # TVV  at 70 kg (L)
    tvq      <- c(0,  4)   # TVQ  at 70 kg (L/h)
    tvvp     <- c(0,  5)   # TVVp at 70 kg (L)
    eta.cl + eta.v ~ c(0.05,
                       0.03, 0.04)   # IIV reduced vs run2 (WT explains variability)
    eta.vp   ~ 0.20
    prop.err <- c(0, 0.10)
    add.err  <- c(0, 30)
  })
  model({
    cl <- tvcl * (WT / 70)^0.75 * exp(eta.cl)
    v  <- tvv  * (WT / 70)      * exp(eta.v)
    q  <- tvq  * (WT / 70)^0.75
    vp <- tvvp * (WT / 70)      * exp(eta.vp)
    cp <- linCmt() * 1000   # ng/mL
    cp ~ add(add.err) + prop(prop.err)
  })
}
