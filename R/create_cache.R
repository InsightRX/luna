#' Create a cache environment to store settings for project and system
#'
#' @param project luna project object
#' @param verbose verbose output?
#'
create_cache <- function(
  project,
  verbose = FALSE
) {

  name <- project$metadata$name
  folder <- project$metadata$folder

  ## Create cache (in global environment)
  cache_folder <- file.path(folder, paste0(".luna.cache.", name))
  .luna_cache <<- cachem::cache_disk(dir = cache_folder)
  .luna_cache$set("folder", folder)
  .luna_cache$set("name", name)

  ## read project settings
  if(verbose)
    cli::cli_alert_info("Reading project settings into cache")
  .luna_cache$set(
    "settings", list()
    ## TODO
  )

  ## store project data
  if(verbose)
    cli::cli_alert_info("Reading runs and results into cache")
  .luna_cache$set(
    "project", project
  )

  ## read pharmpy configuration
  if(verbose)
    cli::cli_alert_info("Reading Pharmpy settings into cache")
  .luna_cache$set(
    "pharmpy_conf", get_pharmpy_conf()
  )

  ## read / update models and results
  update_cache(name = name, folder = folder)

}
