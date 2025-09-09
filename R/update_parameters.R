#' Update parameter estimates (and fix)
#'
#' For example for using model in simulations.
#'
#' @inheritParams attach_fit_info
#' @param fix fix the estimates?
#'
#' @export
#'
#'
update_parameters <- function(
    model,
    fit,
    fix = FALSE,
    verbose = FALSE
) {
  final_model <- attr(fit, "model")
  params <- fit$parameter_estimates
  if(is.null(params)) {
    cli::cli_abort("No parameter estimates found in fit object; cannot update model.")
  }
  if(all(is.nan(params))) {
    cli::cli_alert_warning("No parameter estimates were available, not updating model.")
    return(invisible())
  }
  if(any(is.nan(params))) {
    params <- params[!is.nan(params)]
    cli::cli_alert_info("Only some parameters were estimated, updating only for {names(params)}.")
  }
  if(fix) {
    model <- pharmr::fix_parameters_to(
      model,
      params
    )
  } else {
    model <- pharmr::set_initial_estimates(
      model = model,
      inits = params
    )
  }
  model
}
