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
  tool,
  method = c("psn", "pharmpy"),
  force = FALSE,
  console = FALSE,
  verbose = TRUE
) {
  method <- match.arg(method)
  is_luna_cache_available(abort = TRUE)

  ## Make sure we have up-to-date project info
  luna_load_project(
    name = .luna_cache$get("name"),
    folder = .luna_cache$get("folder"),
    verbose = FALSE
  )

  # Transform folder path to absolute path
  folder <- .luna_cache$get("folder")
  folder <- normalizePath(folder, mustWork = TRUE)

  # Load project data
  project <- .luna_cache$get("project")
  run <- pluck_entry(project$yaml$runs, id = id)
  tool_obj <- pluck_entry(run$tools, id = tool, el = "tool")
  if(is.null(tool_obj)) {
    cli::cli_abort("Tool `{tool}` not defined for `{id}`")
  }

  if(method == "pharmpy") {
    ## Currently no check for PsN tools, since there are so many
    allowed_tools <- c("bootstrap", "vpc")
    if(!tool_obj$tool %in% allowed_tools) {
      cli::cli_abort("Requested tool {tool_obj$tool} not currently available in luna.")
    }
  }

  ## Read the model file and dataset
  model <- pharmr::read_model(file.path(folder, paste0(id, ".mod")))
  data <- model$dataset

  ## Create a run folder
  run_folder <- create_run_folder(
    id,
    folder,
    tool = tool_obj$tool,
    force = force,
    verbose
  )
  dataset_path <- file.path(run_folder, "data.csv")

  ## Copy modelfile + dataset
  write.csv(data, file = dataset_path, quote=F, row.names=F)
  model_code <- model$code
  model_code <- change_nonmem_dataset(model_code, "data.csv")
  model_file <- file.path(run_folder, paste0(id, ".mod"))
  writeLines(model_code, model_file)

  ## Determine what to do
  if(method == "psn") {
    options <- parse_psn_options(tool_obj)
    if(is.null(options)) {
      cli::cli_alert_info("No options specified for {tool_obj$tool}")
    } else {
      cli::cli_alert_info(paste0("Running {tool_obj$tool} with arguments: {options}"))
    }
    call_psn(
      model_file = model_file,
      path = run_folder,
      tool = tool_obj$tool,
      options = options,
      console = console,
      verbose = verbose
    )
  } else { ## pharmpy
    if(tool_obj$tool == "bootstrap") {
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

}
