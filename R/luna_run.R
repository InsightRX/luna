#' Run a NONMEM model
#'
#' @param id run id, e.g. `run1`. This will be the folder in which the NONMEM
#' model is run.
#' @param folder path to folder containing the model file. Default is current directory.
#' @param as_job run as an RStudio job (async), or in the console. If left `NULL`
#' will use setting in luna config.
#'
#' @export
luna_run <- function(
    id,
    folder = NULL,
    nmfe = NULL,
    as_job = NULL,
    ...
) {
  
  id <- unlist(lapply(id, validate_id))
  if(length(id) > 1) {
    cli::cli_abort("Sorry, running of multiple runs in batch is not yet supported.")
  }
  
  ## Get cache and config
  is_luna_cache_available(abort = TRUE)
  config <- get_luna_config()
  name <- .luna_cache$get("project")$metadata$name
  if(is.null(folder)) {
    folder <- .luna_cache$get("project")$metadata$folder
  }
  as_job <- is_run_as_job(config, as_job)
  
  ## make sure we're up to date
  luna_load_project(
    name = name,
    folder = folder,
    verbose = FALSE
  )
  
  # Transform folder path to absolute path
  folder <- normalizePath(folder, mustWork = TRUE)
  
  # read the model file with nm_read_model()
  model_file <- file.path(folder, paste0(id, ".mod"))
  if(! file.exists(model_file)) {
    cli::cli_abort("Model file for run {id} not found!")
  }
  model <- pharmr::read_model(model_file)
  
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
  if(is.null(config$tools$modelfit$method)) {
    cli::cli_alert_warning("Default method for modelfit not configured, using pharmpy dispatcher.")
  }
  method <- ifelse0(config$tools$modelfit$method, "pharmpy")
  console <- ifelse0(config$tools$modelfit$console, TRUE)
  if(is.null(nmfe)) {
    nmfe <- ifelse0(config$tools$modelfit$nmfe, config$tools$nonmem$nmfe)
  }
  if(method == "nmfe") {
    if(is.null(nmfe) || identical(trimws(nmfe), "")) {
      cli::cli_abort(
        c(
          "NMFE method selected, but no nmfe path is configured.",
          "i" = "Set {.code tools.modelfit.nmfe} in the luna config, or pass {.arg nmfe} to {.fn luna_run}."
        )
      )
    }
    nmfe <- pharmr.extra:::get_nmfe_location(nmfe)
  }
  pharmr.extra::run_nlme(
    model = model,
    id = id,
    path = folder,
    verbose = TRUE,
    method = method,
    nmfe = nmfe,
    as_job = as_job,
    console = console,
    ...
  )
}