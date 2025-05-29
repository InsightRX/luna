#' Show info for a model run
#'
#' @param id run id, e.g. `run1`. This will be the folder in which the NONMEM
#' model is run.
#' @param folder path to folder containing the model file. Default is current directory.
#'
#' @export
#'
luna_info <- function(
  id,
  folder = ".",
  ...
) {
  id <- validate_id(id)
  model_file <- file.path(folder, paste0(id, ".mod"))
  output_file <- file.path(folder, paste0(id, ".lst"))
  if(! file.exists(model_file)) {
    cli::cli_abort("Model file not found. Please check the model file and run folder.")
  }
  if(! file.exists(output_file)) {
    cli::cli_abort("Results file not found. Please check the model file and run folder.")
  }
  fit <- pharmr::read_modelfit_results(
    esttool = "nonmem",
    path = model_file
  )
  fit_info <- get_fit_info(
    fit,
    path = folder,
    output_file = output_file
  )
  runs <- .luna_cache$get("project")$yaml$runs
  run_info <- pluck_entry(runs$modelfit, id)
  if(!is.null(run_info$reference)) {
    fit_ref <- pharmr::read_modelfit_results(
      esttool = "nonmem",
      path = file.path(folder, paste0(run_info$reference, ".mod"))
    )
    fit_info$reference_run <- run_info$reference
    fit_info$reference_ofv <- fit_ref$ofv
    if(is.numeric(fit_ref$ofv) && is.numeric(fit$ofv)) {
      fit_info$dofv <- fit$ofv - fit_ref$ofv
    }
  }
  attr(fit, "info") <- fit_info

  ## attach model object
  model <- pharmr::read_model(
    path = model_file
  )
  attr(fit, "model") <- model

  fit
}
