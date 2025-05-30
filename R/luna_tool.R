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

  # Transform folder path to absolute path
  folder <- .luna_cache$get("folder")
  folder <- normalizePath(folder, mustWork = TRUE)

  # Load project data
  project <- .luna_cache$get("project")
  run <- pluck_entry(project$yaml$runs, id = id)
  tool_obj <- pluck_entry(run$tool, id = tool)
  if(is.null(tool_obj)) {
    cli::cli_abort("Tool with id `{tool}` not defined for run `{id}`")
  }

  allowed_tools <- c("bootstrap", "vpc")
  if(!tool_obj$tool %in% allowed_tools) {
    cli::cli_abort("Requested tool {tool_obj$tool} not currently available in luna.")
  }

  ## Determine what to do
  if(tool_obj$tool == "bootstrap") {
    model <- pharmr::read_model(file.path(folder, paste0(id, ".mod")))
    ## TODO: add integrity checks
    out <- pharmr::run_bootstrap(
      model,
      resamples = diag_obj$samples
    )
  } else if (tool_obj$tool == "vpc") {
    ## TODO
    out <- pharmr::run_simulation(
      model
    )
  } else {
    cli::cli_abort("Requested tool {tool_obj$tool} not currently available in luna.")
  }

}
