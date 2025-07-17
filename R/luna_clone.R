#' Clone model
#'
#' @inheritParams luna_run
#' @param new_id new run id
#'
#' @export
#'
luna_clone <- function(
  id,
  new_id,
  folder = NULL,
  force = FALSE,
  verbose = TRUE
) {
  id <- validate_id(id)
  new_id <- validate_id(new_id)
  if(is.null(folder)) {
    folder <- .luna_cache$get("project")$metadata$folder
  }
  old_file <- file.path(folder, paste0(id, ".mod"))
  new_file <- file.path(folder, paste0(new_id, ".mod"))
  if(file.exists(new_file)) {
    if(force) {
      if(verbose) {
        cli::cli_alert_info("File {new_file} exists, overwriting.")
      }
    } else {
      cli::cli_abort("New file {new_file} exists, use `force=TRUE` to overwrite.")
    }
  }
  if(verbose) {
    cli::cli_alert_info("Cloning model {id} to {new_id}.")
  }
  file.copy(
    old_file,
    new_file
  )
  
  ## Update yaml
  if(verbose) {
    cli::cli_alert_info("Updating project YAML")
  }
  is_luna_cache_available(abort = TRUE)
  ## make sure we're up to date
  name <- .luna_cache$get("project")$metadata$name
  folder <- .luna_cache$get("project")$metadata$folder
  luna_load_project(
    name = name,
    folder = folder,
    verbose = FALSE
  )
  
  ## Update cache
  project <- .luna_cache$get("project")
  entry <- pluck_entry(project$yaml$runs, new_id, "id")
  if(is.null(entry)) {
    project$yaml$runs[[length(project$yaml$runs) + 1]] <- list(
      id = new_id,
      description = paste0("Cloned from ", id),
      reference = id
    )
    .luna_cache$set("project", project)
    
    ## Write yaml back to disk
    yaml_file <- file.path(folder, paste0(name, ".yaml"))
    yaml::write_yaml(project$yaml, file = yaml_file)
    if(verbose) {
      cli::cli_alert_success("New model {new_id} added")
    }
  } else {
    cli::cli_alert_warning("Run already exists in YAML, not updating.")
  }

}