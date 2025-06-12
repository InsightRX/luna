#' Run external tools (e.g. diagnostics) on models or run outputs
#' This can be used e.g. for bootstraps and VPCs. The function is implemented
#' in a modular way so that it can be easily extended.
#'
#' @param id run id
#' @param tool id for the tool, needs to be referenced in project YAML. See examples
#' for further details.
#'
#' @export
#'
luna_tool <- function(
  id,
  tool
) {

  is_luna_cache_available(abort = TRUE)
  config <- get_luna_config()
  folder <- .luna_cache$get("folder")

  # Transform folder path to absolute path
  folder <- normalizePath(folder, mustWork = TRUE)

  # Load project data
  project <- .luna_cache$get("project")
  run <- pluck_entry(project$yaml$runs, id = id)
  tool_obj <- pluck_entry(run$tools, id = tool, el = "tool")
  if(is.null(tool_obj)) {
    cli::cli_abort("No `{tool}` run defined for run `{id}`")
  }

  allowed_tools <- c("bootstrap", "vpc", "sim")
  if(!tool_obj$tool %in% allowed_tools) {
    cli::cli_abort("Requested tool {tool_obj$tool} not currently available in luna.")
  }

  ## Load model and results
  model <- pharmr::read_model(file.path(folder, paste0(id, ".mod")))
  model_run_file <- file.path(folder, paste0(id, "/", "run.mod"))
  if(!file.exists(model_run_file)) {
    cli::cli_abort("No run files found for run {id}. Please first run the model before requesting a {tool}.")
  }
  results <- pharmr::read_modelfit_results(model_run_file)

  ## Determine what to do
  if(tool_obj$tool == "bootstrap") {
    out <- pharmr::run_bootstrap(
      model,
      resamples = tool_obj$samples
    )
  } else if (tool_obj$tool %in% c("sim", "vpc")) {
    model <- pharmr::set_simulation(
      model,
      n = tool_obj$samples
    )
    out <- pharmr::run_simulation(
      model
    )
  } else {
    cli::cli_abort("Requested tool {tool_obj$tool} not currently available in luna.")
  }

}
