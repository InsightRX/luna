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
  return_object = FALSE,
  ...
) {
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
  attr(fit, "info") <- fit_info

  ## attach model object
  model <- pharmr::read_model(
    path = model_file
  )
  attr(fit, "model") <- model

  if(return_object) {
    return(fit)
  } else {
    print(fit)
  }
}
