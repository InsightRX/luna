# run5 - 2-cmt + WT allometry + IOV on CL (final model)
# Variability refinement from run3: add inter-occasion variability on CL
# 4 occasions corresponding to the 4 daily doses (24-h intervals)
# IOV implemented as occasion-specific ETAs selected by TIME
# All IOV ETAs share the same initial variance (estimated separately)

run5_model <- function() {
  ini({
    tvcl         <- 11
    tvv          <- 50
    tvq          <-  4
    tvvp         <-  5
    eta.cl + eta.v ~ c(0.05,
                       0.03, 0.04)
    eta.vp       ~ 0.20
    # IOV: 4 occasion-specific ETAs on CL
    eta.iov.cl.1 ~ 0.01   # dose 1 (t < 24 h)
    eta.iov.cl.2 ~ 0.01   # dose 2 (24 ≤ t < 48 h)
    eta.iov.cl.3 ~ 0.01   # dose 3 (48 ≤ t < 72 h)
    eta.iov.cl.4 ~ 0.01   # dose 4 (t ≥ 72 h)
    prop.err     <- 0.10
    add.err      <- 30
  })
  model({
    # Select IOV eta based on dose occasion (defined by 24-h TIME bins)
    if (t < 24) {
      iov_cl <- eta.iov.cl.1
    } else if (t < 48) {
      iov_cl <- eta.iov.cl.2
    } else if (t < 72) {
      iov_cl <- eta.iov.cl.3
    } else {
      iov_cl <- eta.iov.cl.4
    }
    cl <- tvcl * (WT / 70)^0.75 * exp(eta.cl + iov_cl)
    v  <- tvv  * (WT / 70)      * exp(eta.v)
    q  <- tvq  * (WT / 70)^0.75
    vp <- tvvp * (WT / 70)      * exp(eta.vp)
    cp <- linCmt() * 1000
    cp ~ add(add.err) + prop(prop.err)
  })
}
