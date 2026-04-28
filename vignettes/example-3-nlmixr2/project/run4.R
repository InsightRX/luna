# run4 - 2-cmt + WT allometry + SEX effect on CL (tested, not adopted)
# Covariate test from run3: add sex effect on CL
# SEX: 0 = female (reference), 1 = male
# fsex_cl initialised at 0 (no effect); estimated fractional shift
# Expected result: not significant (dOFV < 3.84, 1 df); revert to run3

run4_model <- function() {
  ini({
    tvcl     <- 11
    tvv      <- 50
    tvq      <-  4
    tvvp     <-  5
    fsex_cl  <-  0        # fractional sex difference in CL (male vs female)
    eta.cl + eta.v ~ c(0.05,
                       0.03, 0.04)
    eta.vp   ~ 0.20
    prop.err <- 0.10
    add.err  <- 30
  })
  model({
    fsex <- 1 + fsex_cl * SEX     # SEX: 0 = female (ref), 1 = male
    cl   <- tvcl * (WT / 70)^0.75 * fsex * exp(eta.cl)
    v    <- tvv  * (WT / 70)      * exp(eta.v)
    q    <- tvq  * (WT / 70)^0.75
    vp   <- tvvp * (WT / 70)      * exp(eta.vp)
    cp   <- linCmt() * 1000
    cp ~ add(add.err) + prop(prop.err)
  })
}
