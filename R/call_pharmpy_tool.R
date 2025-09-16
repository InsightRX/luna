#' Generic function for running a pharmpy tool, like bootstrap,
#' or modelsearch. A separate function is available for `fit()`
#'
#' @param model Pharmpy model object, preferably created using
#' `luna::create_model()`.
#' @param id model id. Optional. If not specified, will generate random modelfit
#' id. The `id` will be used to create the run folder.
#' @param verbose verbose output?
#' @param clear if one or more run folders exists for the tool,
#' do we want to remove them first?
#' @param options list of arguments pass on to `tool` as argument. Documentation
#' for available arguments for each Pharmpy tool can be found here:
#' https://pharmpy.github.io/latest/mfl.html.
#'
#' @return fit object
#'
#' @export
#'
call_pharmpy_tool <- function(
  id,
  model = NULL,
  results = NULL,
  tool = NULL,
  folder = NULL,
  clean = TRUE,
  verbose = TRUE,
  force = FALSE,
  options = list()
) {

  if(is.null(tool)) {
    cli::cli_abort("Please provide Pharmpy `tool` to run.")
  }
  if(is.null(model) && is.null(results)) {
    cli::cli_abort("Please provide `model` and/or `results` to start Pharmpy tool.")
  }
  if(is.null(model)) {
    if(!is.null(attr(results, "model"))) {
      if(verbose)
        cli::cli_alert_info("No `model` provided, taking from `results` object")
      model <- attr(results, "model")
    } else {
      cli::cli_abort("Please provide `model` to start Pharmpy tool.")
    }
  }

  ## Check results, if needed
  req_results <- c("modelsearch", "covsearch", "iivsearch", "ruvsearch", "amd")
  if(is.null(results) && tool %in% req_results) {
    if(verbose)
      cli::cli_alert_info("No `results` provided, running the model first to generate `results` object.")
    results <- run_nlme(
      id = id,
      model = model,
      force = force
    )
  }

  ## Prepare run folder
  if(is.null(folder)) {
    folder <- getwd()
  }
  run_folder <- file.path(getwd(), id)
  if(!dir.exists(run_folder))
    run_folder <- create_run_folder(id, folder, force, verbose)

  ## Clean Pharmpy run folders, if requested
  clean_pharmpy_runfolders(id, folder, tool, remove = clean)

  ## Run tool
  if(verbose) {
    cli::cli_alert_info(
      paste0("Starting {tool} in ", run_folder)
    )
  }

  ## Tool-specific modifications / checks
  ##
  ## - simulation: ensure it is a simulation
  if(tool == "simulation") {
    if(verbose)
      cli::cli_alert_info("Making sure model is a simulation model")
    model <- model |>
      pharmr::set_simulation(n = ifelse0(options$n, 1)) |>
      pharmr::set_name("sim")
    options$n <- NULL
  }

  ## prepare arguments for call
  args <- c(
    list(model = model),
    options
  )
  if(tool %in% req_results) {
    args$results <- results
  }

  ## make the call to the Pharmpy tool
  withr::with_dir(run_folder, {
    res <- do.call(
      paste0("run_", tool),
      envir = asNamespace("pharmr"),
      args = args
    )
  })

  ## Post-processing, tool-specific
  ## Save final model to file, and attach to output object
  if(stringr::str_detect(tool, "(.*search|amd)")) {
    final_model <- update_parameters(res$final_model, res$final_results)
    final_model_code <- final_model$code
    writeLines(
      final_model_code,
      file.path(run_folder, glue::glue("final_{tool}.mod"))
    )
    attr(res, "final_model") <- final_model
  }
  if(tool == "simulation") {
    pharmpy_runfolders <- get_pharmpy_runfolders(
      id = id,
      folder = folder,
      tool = tool
    )
    full_table_path <- file.path(run_folder, tail(pharmpy_runfolders, 1), "models", "sim")
    tables <- get_tables_from_fit(
      model,
      path = full_table_path
    )
    if(verbose) {
      if(length(tables) > 0) {
        cli::cli_alert_info(paste0("Attaching {length(tables)} table", ifelse(length(tables) > 1, "s", ""), " from {tool} to output"))
      } else {
        cli::cli_alert_info("No tables found from {tool} at {full_run_path}")
      }
      attr(res, "tables") <- tables
    }
  }

  res

}
