#' Load a project from yaml and gather results. Project is then stored/updated
#' in .luna_cache
#'
#' @inheritParams luna_new_project
#'
#' @export
#'
luna_load_project <- function(
    name = NULL,
    folder = ".",
    verbose = TRUE
) {

  if(is.null(name)) {
    yaml_files <- stringr::str_sort(
      dir(folder, pattern = ".yaml$"),
      numeric = TRUE
    )
    if(length(yaml_files) == 0) {
      cli::cli_abort("No YAML files found in folder.")
    } else if (length(yaml_files) == 1) {
      name <- stringr::str_replace(yaml_files[1], ".yaml$", "")
      cli::cli_alert_info("Found luna project YAML file for project: {name}")
    } else {
      cli::cli_abort("Multiple YAML files found in folder, please specify project name.")
    }
  }

  if(verbose) cli::cli_alert_info("Reading project YAML file")
  filename <- file.path(folder, paste0(name, ".yaml"))
  yaml_data <- read_yaml_safe(filename)

  project <- list(
    yaml = yaml_data,
    metadata = list(
      name = name,
      folder = folder
    )
  )
  class(project) <- c("luna.project", class(project))

  if(dir.exists(paste0(".luna.cache.", name))) {
    is_cache_available <- is_luna_cache_available(abort = FALSE)
    if(!is_cache_available) {
      if(verbose) cli::cli_alert_info("Reloading luna project cache")
      cache_folder <- file.path(folder, paste0(".luna.cache.", name))
      .luna_cache <<- cachem::cache_disk(dir = cache_folder)
    }
    if(verbose) cli::cli_alert_info("Updating luna project cache")
    update_cache(
      project = project,
      verbose = verbose
    )
  } else {
    if(verbose) cli::cli_alert_info("Creating luna project cache")
    create_cache(
      project = project,
      verbose = verbose
    )
  }

  if(verbose) cli::cli_alert_success("Active project: {name}")

}
