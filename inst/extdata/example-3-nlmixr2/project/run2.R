# run2 - 2-compartment base model, no covariates, proportional+additive RUV
# Structural refinement from run1: 1-cmt → 2-cmt
# Correlated IIV on CL and V (BLOCK 2); diagonal IIV on Vp

run2_model <- function() {
  ini({
    tvcl     <- c(0, 11)   # TVCL (L/h)
    tvv      <- c(0, 50)   # TVV  central (L)
    tvq      <- c(0, 4)    # TVQ  inter-compartmental (L/h)
    tvvp     <- c(0, 5)    # TVVp peripheral (L)
    eta.cl + eta.v ~ c(0.06,
                       0.04, 0.04)   # BLOCK(2): IIV CL, cov CL-V, IIV V
    eta.vp   ~ 0.20        # IIV Vp (diagonal)
    prop.err <- c(0, 0.10) # proportional RUV
    add.err  <- c(0, 30)   # additive RUV (ng/mL)
  })
  model({
    cl <- tvcl * exp(eta.cl)
    v  <- tvv  * exp(eta.v)
    q  <- tvq
    vp <- tvvp * exp(eta.vp)
    cp <- linCmt() * 1000   # ng/mL
    cp ~ add(add.err) + prop(prop.err)
  })
}
