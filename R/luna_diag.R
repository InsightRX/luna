luna_diag <- function(
  id,
  diagnostic
) {
  is_luna_cache_available(abort = TRUE)

  # Transform folder path to absolute path
  folder <- .luna_cache$get("folder")
  folder <- normalizePath(folder, mustWork = TRUE)

  # Load project data
  project <- .luna_cache$get("project")
  run <- pluck_entry(project$yaml$runs, id = id)
  diag_obj <- pluck_entry(run$diagnostics, id = diagnostic)
  if(is.null(diag_obj)) {
    cli::cli_abort("Diagnostic with id `{diagnostic}` not defined for run `{id}`")
  }

  allowed_tools <- c("bootstrap", "vpc")
  if(!diag_obj$tool %in% allowed_tools) {
    cli::cli_abort("Requested diagnostic {diag_obj$tool} not currently available in luna.")
  }

  ## Determine what to do
  if(diag_obj$tool == "bootstrap") {
    model <- pharmr::read_model(file.path(folder, paste0(id, ".mod")))
    ## TODO: add integrity checks
    out <- pharmr::run_bootstrap(
      model,
      resamples = diag_obj$samples
    )
  } else if (diag_obj$tool == "vpc") {
    ## TODO
    out <- pharmr::run_simulation(
      model
    )
  } else {
    cli::cli_abort("Requested diagnostic {diag_obj$tool} not currently available in luna.")
  }

}
