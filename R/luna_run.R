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
  run_nlme(
    model = model,
    id = id,
    path = folder,
    verbose = TRUE,
    method = method,
    as_job = as_job,
    console = console,
    ...
  )
}

#' Helper function to determine nmfe location from various sources
#' The order is as follows:
#'
#' 1. argument specified by user
#' 2. check project settings (not implemented for now, will add later)
#' 3. check pharmpy config
#' 4. throw error, force user to specify
#'
get_nmfe_location_for_run <- function(nmfe = NULL, verbose = FALSE) {
  if(!is.null(nmfe)) {
    if(verbose) cli::cli_alert_info("Using user-specified NONMEM version at {nmfe}")
  } else {
    pharmpy_conf <- get_pharmpy_conf()
    nm_path <- pharmpy_conf$pharmpy.plugins.nonmem$default_nonmem_path
    if(is.null(nm_path)) {
      cli::cli_abort("Pharmpy is not configured to run NONMEM.")
    }
    if(!file.exists(nm_path)) {
      cli::cli_abort("NONMEM path configured in Pharmpy is not a valid folder. Please reconfigure Pharmpy")
    }
    nmfe_file <- detect_nmfe_version(nm_path)
    nmfe <- file.path(nm_path, "run", nmfe_file)
    if(verbose) cli::cli_alert_info("Using Pharmpy-configured NONMEM version at {nmfe}")
  }
  if(is.null(nmfe) || !file.exists(nmfe)) {
    cli::cli_abort("Could not determine path to NONMEM nmfe file, please specify manually with `nmfe` argument or reconfigure Pharmpy.")
  }
  nmfe
}

#' get nmfe file name from a NONMEM installation folder
#'
detect_nmfe_version <- function(nm_path) {
  files <- dir(
    file.path(nm_path, "run"),
    pattern = "nmfe\\d\\d"
  )
  files[1]
}
