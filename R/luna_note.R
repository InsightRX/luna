#' Annotate a run
#' Either add a note or clear notes from a run
#'
#' @inheritParams luna_run
#' @param note note(s) to add (character vector)
#' @param clear should existing notes be cleared? Default is `FALSE`
#'
#' @examples
#' # example code
#'
#' luna_note("run1", "Initial model") # adds a note
#' luna_note("run1", "Initial model", clear=TRUE) # first clears existing notes, then adds a note
#' luna_note("run1", clear=TRUE) # only clears existing notes
#'
#' @export
#'
luna_note <- function(
  id,
  note = NULL,
  clear = FALSE,
  force = TRUE,
  verbose = TRUE
) {

  is_luna_cache_available(abort = TRUE)

  ## make sure we're up to date
  name <- .luna_cache$get("project")$metadata$name
  folder <- .luna_cache$get("project")$metadata$folder
  luna_load_project(
    name = name,
    folder = folder,
    verbose = FALSE
  )

  ## update yaml and cache
  if(verbose)
    cli::cli_alert_info("Reading {name} project file and finding data for {id}")
  project <- .luna_cache$get("project")
  run <- pluck_entry(project$yaml$runs$modelfit, id)
  run$notes <- c(
    run$notes,
    note
  )

  if(!clear && is.null(note)) {
    cli::cli_abort("Either use `note` or `clear=TRUE` to set or clear notes.")
  }

  ## Clear existing notes
  if(clear) {
    run$notes <- NULL
  }

  ## Add note
  if(!is.null(note)) {
    project$yaml$runs$modelfit <- insert_entry(
      x = project$yaml$runs$modelfit,
      id = id,
      entry = run
    )
  }

  ## Update cache
  .luna_cache$set("project", project)

  ## Write yaml back to disk
  yaml_file <- file.path(folder, paste0(name, ".yaml"))
  yaml::write_yaml(project$yaml, file = yaml_file)
  if(verbose)
    cli::cli_alert_success("Notes updated for {id}")

}
