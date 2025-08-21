#' Update parameter estimates (and fix)
#'
#' For example for using model in simulations.
#'
#' @inheritParams attach_fit_info
#' @param fix fix the estimates? Currently this can only be `TRUE`, since this is the
#' only option that Pharmpy/pharmr supports.
#'
#' @export
#'
update_parameters <- function(
    model,
    fit,
    fix = TRUE,
    verbose = TRUE
) {
  final_model <- attr(fit, "model")
  params <- fit$parameter_estimates
  if(is.null(params)) {
    cli::cli_abort()
  }
  if(all(is.nan(params))) {
    cli::cli_alert_warning("No parameter estimates were available, not updating model.")
    return(invisible())
  }
  if(any(is.nan(params))) {
    params <- params[!is.nan(params)]
    cli::cli_alert_info("Only some parameters were estimated, updating only for {names(params)}.")
  }
  model <- pharmr::fix_parameters_to(
    model,
    params
  )
  model
}
