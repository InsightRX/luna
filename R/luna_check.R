#' Syntax-check a NONMEM model
#'
#' Uses pharmpy to parse the model. A successful parse indicates the model is
#' valid. Any pharmpy parse error is reported as a syntax issue.
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
  
  folder <- normalizePath(folder, mustWork = TRUE)
  
  model_file <- file.path(folder, paste0(id, ".mod"))
  if (!file.exists(model_file)) {
    cli::cli_abort("Model file for run {id} not found!")
  }
  
  model <- tryCatch(
    pharmr::read_model(model_file),
    error = function(e) {
      cli::cli_alert_warning("Model has a syntax error:")
      message(conditionMessage(e))
      return(invisible(FALSE))
    }
  )
  
  if (isFALSE(model)) return(invisible(FALSE))
  
  if (!inherits(model, "pharmpy.model.model.Model")) {
    cli::cli_alert_warning("Model could not be parsed as a pharmpy model.")
    return(invisible(FALSE))
  }
  
  if (is.null(model$dataset)) {
    cli::cli_alert_warning("Model parsed but dataset could not be loaded. Check the {.field $DATA} path.")
    return(invisible(FALSE))
  }

  cli::cli_alert_success("Model loaded successfully.")

  model_ok <- pharmr.extra::run_nlme(
    model,
    check_only = TRUE,
    verbose = verbose
  )

  if (isFALSE(model_ok)) {
    cli::cli_alert_warning("Model failed NONMEM compilation check.")
    return(invisible(FALSE))
  }

  cli::cli_alert_success("Model syntax OK!")
  invisible(TRUE)
}
