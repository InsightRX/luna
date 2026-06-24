#' Run a model
#'
#' Dispatches to the configured execution backend. Supported methods:
#' `"pharmpy"` (default), `"psn"`, `"nmfe"` for NONMEM-based workflows, and
#' `"ferx"` for the ferx-nlme Rust engine.
#'
#' @param id run id, e.g. `run1`. This will be the folder in which the model
#' is run.
#' @param folder path to folder containing the model file. Default is current directory.
#' @param as_job run as an RStudio job (async), or in the console. If left `NULL`
#' will use setting in luna config. Not yet supported for `method = "ferx"`.
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

  ## Detect method early to dispatch non-NONMEM engines
  method <- ifelse0(config$tools$modelfit$method, "pharmpy")

  if (method == "ferx") {
    return(luna_run_ferx(id = id, folder = folder, config = config, ...))
  }

  # read the model file with nm_read_model()
  model_file <- file.path(folder, paste0(id, ".mod"))
  if(! file.exists(model_file)) {
    cli::cli_abort("Model file for run {id} not found!")
  }
  model <- pharmr::read_model(model_file)

  # Some integrity checks
  if(! inherits(model, "pharmpy.model.model.Model")) {
    cli::cli_abort("Model is not a pharmpy model. Please check the model file.")
  }
  if(is.null(model$dataset)) {
    cli::cli_abort("Model has no dataset. Please check the model and dataset files.")
  }
  cli::cli_alert_success("Model loaded successfully.")

  # Resolve data file path as string to avoid pandas→R coercion inside run_nlme
  data_path <- get_nm_data_path(model_file)

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

#' Run a ferx model (internal helper for luna_run)
#'
#' @inheritParams luna_run
#' @param config resolved luna config list
#'
#' @keywords internal
luna_run_ferx <- function(id, folder, config, force = FALSE, ...) {
  if (!requireNamespace("ferx", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg ferx} package is required to run ferx models.",
      "i" = "Install it with: {.code devtools::install_github('FeRx-NLME/ferx-r')}"
    ))
  }

  model_file <- file.path(folder, paste0(id, ".ferx"))
  if (!file.exists(model_file)) {
    cli::cli_abort("ferx model file {.file {model_file}} not found!")
  }

  data_path <- config$tools$modelfit$data
  if (is.null(data_path)) {
    cli::cli_abort(c(
      "ferx method requires a data file path.",
      "i" = "Set {.code tools.modelfit.data} in the project config YAML."
    ))
  }
  if (!file.exists(data_path)) {
    data_path_abs <- file.path(folder, data_path)
    if (!file.exists(data_path_abs)) {
      cli::cli_abort(
        "Data file {.file {data_path}} not found (also checked relative to project folder)."
      )
    }
    data_path <- data_path_abs
  }

  ## log event
  log_add(
    event = "action",
    action = "modelfit",
    id = id
  )

  ## Extract method from [fit_options] section if not supplied in ...
  dots <- list(...)
  if (is.null(dots$method)) {
    fit_opt_lines <- tryCatch(
      ferx::ferx_model_section(model_file, "fit_options"),
      error = function(e) character(0)
    )
    method_line <- grep("^\\s*method\\s*=", fit_opt_lines, value = TRUE)
    if (length(method_line) > 0) {
      parsed_method <- trimws(sub(".*=\\s*", "", method_line[1]))
      # strip trailing comments
      parsed_method <- trimws(sub("#.*", "", parsed_method))
      if (nzchar(parsed_method)) {
        dots$method <- parsed_method
        cli::cli_alert_info("Using method from .ferx file: {.val {parsed_method}}")
      }
    }
  }

  cli::cli_alert_info("Running ferx model {.val {id}}...")

  result <- do.call(ferx::ferx_fit,
                    c(list(model = model_file, data = data_path), dots))

  result_file <- file.path(folder, paste0(id, "-fit.rds"))
  saveRDS(result, result_file)

  if (isTRUE(result$converged)) {
    cli::cli_alert_success(
      "ferx run {.val {id}} converged. OFV = {round(result$ofv, 2)}"
    )
  } else {
    cli::cli_alert_warning("ferx run {.val {id}} did not converge.")
  }
  cli::cli_alert_info("Results saved to {.file {result_file}}")

  invisible(result)
}