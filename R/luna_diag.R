luna_diag <- function(
  id,
  diagnostic
) {
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
    ## TODO
  } else if (diag_obj$tool == "vpc") {
    ## TODO
  } else {
    cli::cli_abort("Requested diagnostic {diag_obj$tool} not currently available in luna.")
  }

}
