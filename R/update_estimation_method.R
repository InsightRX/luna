#' Wrapper around pharmr's functions to set/add estimation methods
#'
#' The current pharmpy functionality is not stable, hence the need for this
#' wrapper.
#'
#' @inheritParams run_nlme
#'
update_estimation_method <- function(
    model,
    estimation_method,
    verbose = TRUE
) {
  estimation_method <- toupper(estimation_method)
  allowed <- c("FO", "FOCE", "ITS", "LAPLACE", "IMPMAP", "IMP", "SAEM")
  if(length(estimation_method) > 1) {
    cli::cli_alert_warning("Currently setting estimation methods supports only a single estimation method. Using {estimation_method[1]}")
    estimation_method <- estimation_method[1]
  }
  if(any(! estimation_method %in% allowed)) {
    cli::cli_abort("The requested estimation method was not recognized. Available estimation methods are {allowed} or their lower-case equivalents.")
  }
  ## Due to an issue with indexing in Pharmpy, we can currently only
  ## override existing estimation steps, not add new ones.
  existing_steps <- model$execution_steps$to_dataframe()
  n_existing <- nrow(existing_steps)
  for(i in seq_along(estimation_method)) {
    model <- pharmr::set_estimation_step(
      model,
      method = estimation_method[i],
      idx = 0
    )
    if(verbose) {
      cli::cli_alert_info("Setting estimation method to {estimation_method}")
    }
  }
  n_remove <- nrow(model$execution_steps$to_dataframe()) - n_existing
  if(n_remove > 1) {
    for(i in 1:n_remove) {
      model <- pharmr::remove_estimation_step(model, i)
    }
  }
  model
}
