#' Load a project from yaml and gather results
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
      cli::cli_alert_info("Found Luna project file for project: {name}")
    } else {
      cli::cli_abort("Multiple YAML files found in folder, please specify project name.")
    }
  }

  if(verbose) cli::cli_alert_info("Reading project file")
  filename <- file.path(folder, paste0(name, ".yaml"))
  yaml_data <- yaml::read_yaml(file = filename)

  luna_project <- list(
    yaml = yaml_data,
    metadata = list(
      name = name,
      folder = folder
    )
  )
  class(luna_project) <- c("luna.project", class(luna_project))

  if(verbose) cli::cli_alert_info("Creating / updating luna cache")
  create_luna_cache(
    folder = folder,
    name = name,
    verbose = verbose
  )

  luna_project
}
