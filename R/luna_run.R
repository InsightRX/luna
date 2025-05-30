#' Run a NONMEM model
#'
#' @param id run id, e.g. `run1`. This will be the folder in which the NONMEM
#' model is run.
#' @param folder path to folder containing the model file. Default is current directory.
#' @param diagnostic run a diagnostic for the run, like a bootstrap or vpc. The
#' id `diagnostic` needs to be defined in the project yaml file.
#'
#' @export
luna_run <- function(
  id,
  folder,
  method = NULL,
  diagnostic = NULL,
  verbose = TRUE,
  ...
) {

  ## Get cache and config
  is_luna_cache_available(abort = TRUE)
  config <- get_luna_config()
  folder <- .luna_cache$get("folder")

  # Transform folder path to absolute path
  folder <- normalizePath(folder, mustWork = TRUE)

  # read the model file with nm_read_model()
  model <- pharmr::read_model(file.path(folder, paste0(id, ".mod")))

  # Some integrity checksa
  if(! inherits(model, "pharmpy.model.model.Model")) {
    cli::cli_abort("Model is not a pharmpy model. Please check the model file.")
  }
  if(is.null(model$dataset)) {
    cli::cli_abort("Model has no dataset. Please check the model and dataset files.")
  }
  cli::cli_alert_success("Model loaded successfully.")

  ## log event
  log_add(
    event = "action",
    action = "modelfit",
    id = id
  )

  # Determine nmfe location to use.
  if(is.null(method)) {
    method <- config$tools$modelfit$method
    if(is.null(method)) {
      cli::cli_abort("Run method not specified")
    }
  } else {
    if(verbose) cli::cli_alert_info("Using {method} to run")
  }
  fit <- run_nlme(
    model = model,
    id = id,
    path = folder,
    verbose = TRUE,
    method = method,
    ...
  )

  fit
}
