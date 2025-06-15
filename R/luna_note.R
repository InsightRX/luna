#' Annotate a run
#' Either add a note or clear notes from a run
#'
#' @inheritParams luna_run
#' @param note note(s) to add (character vector)
#' @param clear should existing notes be cleared? Default is `FALSE`
#' @param element what element in the YAML contains the notes, default is `notes`.
#'
#' @examples
#' \dontrun{
#' luna_note("run1", "Initial model") # adds a note
#' luna_note("run1", "Initial model", clear=TRUE) # first clears existing notes, then adds a note
#' luna_note("run1", clear=TRUE) # only clears existing notes
#' }
#'
#' @export
#'
luna_note <- function(
  id = NULL,
  note = NULL,
  clear = FALSE,
  force = TRUE,
  verbose = FALSE,
  element = "notes"
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
  if(is.null(id)) {
    cli::cli_abort("Please specify an `id` to read or update metadata for.")
  }
  if(verbose)
    cli::cli_alert_info("Reading {name} project file and finding data for {id}")
  project <- .luna_cache$get("project")
  run <- pluck_entry(project$yaml$runs, id)

  if(!clear && is.null(note)) { # just show current notes / tags
    if(is.null(run[[element]])) {
      cli::cli_alert_info("No entry found for {id}.")
    }
    return(run[[element]])
  } else { # add or clear notes/tags

    ## Clear existing notes
    if(clear) {
      run[[element]] <- NULL
    }

    ## Add note
    run[[element]] <- c(
      run[[element]],
      note
    )
    if(!is.null(note)) {
      project$yaml$runs <- insert_entry(
        x = project$yaml$runs,
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
      cli::cli_alert_success("{element} updated for {id}")

  }

}
