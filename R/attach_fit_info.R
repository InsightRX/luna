#' Attach fit info and tables to a fit object, e.g. from model fit or
#' Pharmpy grid search final results
#'
#' @inheritParams run_nlme
#' @inheritParams get_fit_info
#'
#' @export
#'
attach_fit_info <- function(
  fit,
  model,
  fit_folder,
  output_file = file.path(fit_folder, "model.lst"),
  is_sim_model = FALSE,
  verbose = TRUE
) {
  ## Attach model object (with dataset) to fit, for traceability or use in post-processing
  attr(fit, "model") <- model

  ## Attach tables to model fit
  if(verbose) cli::cli_process_start("Importing generated tables")
  tables <- get_tables_from_fit(
    model,
    fit_folder
  )
  attr(fit, "tables") <- tables
  if(verbose) cli::cli_process_done()

  if(!is_sim_model) {
    ## Generate a summary of fit info
    if(verbose) cli::cli_process_start("Summarizing fit results")
    fit_info <- get_fit_info(
      fit,
      path = fit_folder,
      output_file = output_file
    )
    attr(fit, "info") <- fit_info
  }

  if(verbose) cli::cli_process_done()

  fit
}
