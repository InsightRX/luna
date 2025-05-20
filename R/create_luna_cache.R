#' Create a cache environment to store settings for project and system
#'
#' @param folder path to project
#' @param name optional, name of project (matching to yaml file)
#'
create_cache <- function(
  name,
  folder,
  verbose = FALSE
) {

  ## Create cache (in global environment)
  cache_folder <- file.path(folder, paste0(".luna.cache.", name))
  .luna_cache <<- cachem::cache_disk(dir = cache_folder)
  .luna_cache$set("folder", folder)
  .luna_cache$set("name", name)

  ## read project settings
  if(verbose)
    cli::cli_alert_info("Reading project settings into cache")
  .luna_cache$set(
    "project", list()
    ## TODO
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
