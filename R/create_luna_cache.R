#' Create a cache environment to store settings for project and system
#'
#' @param folder path to project
#' @param name optional, name of project (matching to yaml file)
#'
create_luna_cache <- function(
  folder,
  name = NULL,
  verbose = FALSE
) {

  ## Create cache (in global environment)
  .luna_cache <<- cachem::cache_mem()
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
}
