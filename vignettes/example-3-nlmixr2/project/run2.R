# run2 - 2-compartment base model, no covariates, proportional+additive RUV
# Structural refinement from run1: 1-cmt → 2-cmt
# Correlated IIV on CL and V (BLOCK 2); diagonal IIV on Vp

run2_model <- function() {
  ini({
    tvcl     <- 11        # TVCL (L/h)
    tvv      <- 50        # TVV  central (L)
    tvq      <-  4        # TVQ  inter-compartmental (L/h)
    tvvp     <-  5        # TVVp peripheral (L)
    eta.cl + eta.v ~ c(0.06,
                       0.04, 0.04)   # BLOCK(2): IIV CL, cov CL-V, IIV V
    eta.vp   ~ 0.20       # IIV Vp (diagonal)
    prop.err <- 0.10      # proportional RUV
    add.err  <- 30        # additive RUV (ng/mL)
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
