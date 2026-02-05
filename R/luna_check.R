#' Syntax-check a NONMEM model
#'
#' @inheritParams luna_run
#'
#' @export
luna_check <- function(
  id,
  folder = NULL,
  verbose = FALSE,
  ...
) {

  id <- unlist(lapply(id, validate_id))
  if(length(id) > 1) {
    cli::cli_abort("Sorry, checking multiple runs in batch is not yet supported.")
  }

  ## Get cache and config
  is_luna_cache_available(abort = TRUE)
  config <- get_luna_config()
  name <- .luna_cache$get("project")$metadata$name
  if(is.null(folder)) {
    folder <- .luna_cache$get("project")$metadata$folder
  }

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

  model_ok <- pharmr.extra::run_nlme(
    model = model,
    id = id,
    path = folder,
    method = "nmfe",
    as_job = as_job,
    check_only = TRUE, # !! don't run the model, only check it using NM-TRAN
    console = FALSE,
    verbose = verbose,
    ...
  )

  if(model_ok) {
    cli::cli_alert_success("Model syntax OK!")
  } else {
    cli::cli_alert_warning("Model seems to have syntax error")
    cat(attr(model_ok, "message"))
  }

}
