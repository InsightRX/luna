#' Get a very crude estimate for V to serve as initial estimate
#' for CL and V, without performing an NCA. The calculation is based on
#' the assumption that often in clinical trial data, there is at least a
#' peak and a trough (and likely other samples) taken, hence it's
#' possible to get a crude estimate for CL and V from that.
#' For 2-compartment models we just set Q and V to half and
#' twice the size of CL and V, which is often a good starting point.
#' In most scenarios this is sufficiently close to the final estimates that
#' estimation methods will be able to find the global minimum.
#'
#' @param data NONMEM-style dataset
#' @param n_cmt number of distribution / elimination compartments.
#'
#' @export
get_initial_estimates_from_data <- function(
  data,
  n_cmt = 1
) {

  ## TODO: an extension could be to automatically add
  ## observed value scaling, e.g. when V < 1.0.
  pars <- data.frame()
  ids <- unique(data$ID)
  for(id in ids) {
    tmp <- get_initial_estimates_from_individual_data(
      data[data$ID == id,]
    )
    if(length(tmp) > 0) {
      pars <- dplyr::bind_rows(pars, tmp)
    }
  }
  est <- pars |>
    dplyr::summarise_all(function(x) signif(mean(x, na.rm=TRUE), 3)) |>
    as.list()
  if(n_cmt >= 2) {
    est$QP1 <- est$CL
    est$VP1 <- est$V * 2
  }
  if(n_cmt == 3) {
    est$QP2 <- est$CL
    est$VP2 <- est$V * 3
  }

  est
}

#' Core function to get parameter estimates from individual data
#'
get_initial_estimates_from_individual_data <- function(data, ...) {

  dat <- data |>
   dplyr::mutate(dosenr = cumsum(EVID))

  ## Get first dose number for which more than two samples are available.
  dose_nr <- dat |>
    dplyr::filter(MDV == 0) |>
    dplyr::group_by(dosenr) |>
    dplyr::summarise(n_obs = length(TIME)) |>
    dplyr::filter(n_obs >= 2) |>
    dplyr::slice(1) |>
    dplyr::pull(dosenr)

  if(length(dose_nr) == 0) {
    ## take first observation for which at least one obs is available
    dose_nr <- dat |>
      dplyr::filter(MDV == 0) |>
      dplyr::group_by(dosenr) |>
      dplyr::summarise(n_obs = length(TIME)) |>
      dplyr::filter(n_obs == 1) |>
      dplyr::slice(1) |>
      dplyr::pull(dosenr)
  }
  if(length(dose_nr) == 0) { # no observations in data
    return()
  }

  ## get peak value. This leads to estimate for V
  tmp <- dat |>
    dplyr::filter(dosenr == dose_nr & MDV == 0) |>
    dplyr::slice(unique(c(which.max(DV), which.min(DV))))
  dose <- dat |>
    dplyr::filter(dosenr == dose_nr & EVID == 1) |>
    dplyr::pull(AMT)
  est <- c()
  est$V <- dose / max(tmp$DV)
  if(nrow(tmp) > 1) { # two datapoints at least
    KEL <- (log(max(tmp$DV)) - log(min(tmp$DV))) / abs(diff(tmp$TIME))
    est$CL <- KEL * est$V
  } else { # more crude estimation
    est$V <- dose / (max(tmp$DV) * 5)
    est$CL <- est$V / 10
  }

  unlist(est)
}
