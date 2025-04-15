#' Run a NONMEM model
#' 
#' @param id run id, e.g. `run1`. This will be the folder in which the NONMEM
#' model is run.
#' @param folder path to folder containing the model file. Default is current directory.
#' 
#' @export
luna_run <- function(
  id, 
  folder = ".",
  type = "modelfit", 
  ...
) {

  # Transform folder path to absolute path
  folder <- normalizePath(folder, mustWork = TRUE)

  # read the model file with nm_read_model()
  model <- pharmr::read_model(file.path(folder, paste0(id, ".mod")))

  # Some integrity checks
  if(type == "modelfit") {
    if(! inherits(model, "pharmpy.model.model.Model")) {
      cli::cli_abort("Model is not a pharmpy model. Please check the model file.")
    }
    if(is.null(model$dataset)) {
      cli::cli_abort("Model has no dataset. Please check the model and dataset files.")
    }
    cli::cli_alert_success("Model loaded successfully.")
  }

  # Run the model
  fit <- run_nlme(
    model = model,
    id = id,
    path = folder,
    verbose = TRUE,
    ...
  )

  # return the fit
  fit
}