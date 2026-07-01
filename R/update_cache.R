#' Update luna project cache
#'
#' @inheritParams luna_load_project
#'
#' @export
#'
update_cache <- function(
  project,
  verbose = FALSE
) {

  name <- project$metadata$name
  folder <- project$metadata$folder

  ## Check cache
  is_cache_available <- is_luna_cache_available(abort = TRUE)

  ## Save project info
  .luna_cache$set("project", project)

  # Create timestamps object
  timestamps <- list(
    models = list(),
    folders = list()
  )

  ## Models (NONMEM)
  model_list <- dir(folder, pattern = ".mod$", include.dirs = FALSE)
  for(m in model_list) {
    id <- stringr::str_replace(m, "\\.mod$", "")
    timestamps$models[[id]] <- get_time_last_updated_file(
      file.path(folder, m)
    )
    timestamps$results[[id]] <- get_time_last_updated_file(
      file.path(folder, paste0(id, ".lst"))
    )
  }

  ## Models (ferx)
  ferx_list <- dir(folder, pattern = "\\.ferx$", include.dirs = FALSE)
  for(m in ferx_list) {
    id <- stringr::str_replace(m, "\\.ferx$", "")
    timestamps$models[[id]] <- get_time_last_updated_file(
      file.path(folder, m)
    )
    timestamps$results[[id]] <- get_time_last_updated_file(
      file.path(folder, paste0(id, ".fitrx"))
    )
  }

  ## Folders
  dir_list <- stringr::str_replace_all(
    list.dirs(folder, recursive = FALSE),
    paste0(folder, "[\\/]"),
    ""
  )
  for(d in dir_list) {
    timestamps$folders[[d]] <- get_time_last_updated_folder(d)
  }

  if(verbose)
    cli::cli_alert_info("Updating timestamps of models and results")

  .luna_cache$set("timestamps", timestamps)

}
