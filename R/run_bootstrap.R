#' Run bootstrap, wrapper around pharmr::run_bootstrap()
#'
#' Run the model directly using nmfe (not through pharmpy).
#' This is a more reliable way of running NONMEM, and it is now possible to
#' stream stdout and stderr to file or to console, which is useful for
#' inspection of intermediate model fit.
#'
#' The function does take a pharmpy model as input (optionally), and uses
#' pharmpy to read the results from the model fit, and returns a pharmpy
#' `modelfit` object.
#'
#' @inheritParams run_nlme
#'
#' @export
#'
run_bootstrap <- function(
    model,
    id = NULL,
    path = getwd(),
    force = FALSE,
    console = FALSE,
    save_fit = TRUE,
    save_summary = TRUE,
    auto_stack_encounters = TRUE,
    clean = TRUE,
    verbose = TRUE
) {

  time_start <- Sys.time()

  model <- validate_model(model)
  tool <- "nonmem"

  bs <- pharmr::run_bootstrap(
    model = model
  )

  bs

}
