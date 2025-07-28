#' Run external tools (e.g. diagnostics) on models or run outputs
#' This can be used e.g. for bootstraps and VPCs. The function is implemented
#' in a modular way so that it can be easily extended.
#'
#' @inheritParams luna_run
#' @param tool id for the tool, needs to be referenced in project YAML. See examples
#' for further details.
#' @param verbose verbose output
#'
#' @export
#'
luna_tool <- function(
  id,
  tool = NULL,
  force = FALSE,
  as_job = NULL,
  verbose = TRUE
) {

  id <- validate_id(id)

  is_luna_cache_available(abort = TRUE)
  config <- get_luna_config()
  name <- .luna_cache$get("project")$metadata$name
  folder <- .luna_cache$get("project")$metadata$folder
  as_job <- is_run_as_job(config, as_job)

  if(as_job) {
    if(is.null(tool))
      cli::cli_abort("Please specify which `tool` to run as job")
    suppressMessages({
      jobid <- job::job(
        title = paste0(id, "-", tool),
        {
          devtools::load_all("~/git/pharmaai/luna")
          luna::luna_load_project(name, folder)
          luna::luna_tool(
            id,
            tool = tool,
            as_job = FALSE,
            verbose = verbose
          )
        }
      )
    })
    cli::cli_alert_info("Job with id {jobid} started")
    return(invisible(jobid))
  }

  ## make sure we're up to date
  luna_load_project(
    name = name,
    folder = folder,
    verbose = FALSE
  )

  # Transform folder path to absolute path
  folder <- normalizePath(folder, mustWork = TRUE)

  # Load project data
  project <- .luna_cache$get("project")
  run <- pluck_entry(project$yaml$runs, id = id)
  if(is.null(tool)) {
    specified_tools <- unique(unlist(lapply(run$tools, function(x) { x$tool })))
    cli::cli_alert_info("Please specify `tool` to run. Specified tools for this run: {specified_tools}")
    return(invisible())
  }
  tool_obj <- pluck_entry(run$tools, id = tool, el = "tool")
  if(is.null(tool_obj)) {
    cli::cli_abort("No `{tool}` run defined for run `{id}`")
  }

  ## Load model and results
  model <- pharmr::read_model(file.path(folder, paste0(id, ".mod")))
  run_folder <- file.path(folder, id)
  model_run_file <- find_file_with_fallback(
    folder,
    filename = file.path(id, paste0("run", ".mod")),
    fallback = paste0(id, ".mod"),
    verbose = FALSE
  )

  ## Determine method and options
  method <- ifelse0(config$tools[[tool]]$method, "pharmpy")
  if(stringr::str_detect(tool_obj$tool, "::")) {
    full_tool <- stringr::str_split(tool_obj$tool, "::")[[1]]
    method <- tolower(full_tool[1])
  }
  if(!method %in% c("psn", "pharmpy")) {
    cli::cli_abort("Requested tools from {method} not currently supported in luna.")
  }
  options <- tool_obj$options[[1]]

  ## create run folder, if needed
  model <- prepare_run_folder(
    id = id,
    model = model,
    path = folder,
    force = force,
    model_path =  file.path(fit_folder, "run.mod"),
    dataset_path = file.path(fit_folder, "data.csv"),
    verbose = verbose
  )

  ## Determine what to do
  if(method == "pharmpy") {
    results <- pharmr::read_modelfit_results(model_run_file)
    call_pharmpy_tool(
      id = id,
      model = model,
      results = results,
      tool = tool,
      options = options
    )
  } else if(method == "psn") {

    ## Parse options into args
    args <- parse_psn_args(tool_obj)
    tool_clean <- gsub("^(.*?)::", "", tool)

    ## call PsN tool
    if(verbose) {
      cli::cli_alert_info("Running: {tool} on {id} with args: {args}")
    }
    suppressWarnings(
      call_psn(
        model_file = "run.mod",
        path = run_folder,
        options = args,
        tool = tool_clean,
        console = TRUE
      )
    )
  }
}

#' Parse tool options specified in YAML into PsN commandline args
#'
#' @param options list of options. Logical arguments should be specified
#' as TRUE/FALSE.
#'
parse_psn_args <- function(options) {
  options$id <- NULL
  options$tool <- NULL
  if(is.null(options) || length(options) == 0) {
    return(NULL)
  }
  ## split in logical and epxlicit options
  logical_options <- list()
  for(key in names(options)) {
    if(class(options[[key]]) == "logical") {
      logical_options[[key]] <- options[[key]]
      options[[key]] <- NULL
    }
  }
  args <- c(paste0("--", names(options), "=", options))
  if(length(logical_options) > 0) {
    args <- c(args, paste0("--", names(logical_options)))
  }
  args
}
