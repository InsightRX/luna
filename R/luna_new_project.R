#' Create a new project folder with template YAML project file
#'
#' @param name name of the project, will be used as as the filename for the
#' YAML file. Cannot contain spaces.
#' @param description project description
#' @param folder folder to create the project in. If the folder doesn't exists,
#' it will be created.
#' @param import_models when creating a new project file in a folder, should
#' any existing models be added to the project?
#' @param template Create yaml file from a template. Use \link{luna_project_templates}
#' to show a list of available templates
#' @param force if a project file already exists, overwrite it. `FALSE` by default.
#' @param return_object return the project object?
#' @param verbose verbosity
#'
#' @export
#'
luna_new_project <- function(
  name,
  folder = ".",
  description,
  import_models = TRUE,
  force = FALSE,
  template = NULL,
  return_object = FALSE,
  verbose = TRUE
) {

  ## create folder if it doesn't exist
  if(!dir.exists(paths = folder)) {
    if(verbose)
      cli::cli_alert_info(paste0("Creating folder: ", folder))
    dir.create(folder)
  }
  project_yaml_file <- file.path(folder, paste0(name, ".yaml"))
  if(file.exists(project_yaml_file) && !force) {
    cli::cli_abort("Project file exists in this folder. Please specify different project name or folder, or use `force=TRUE` argument.")
  }

  # create a project object from template
  if(!is.null(template)) {
    template <- stringr::str_replace(template, "\\.yaml$", "")
    project <- yaml::read_yaml(file.path(system.file("templates", package = "luna"), paste0(template, ".yaml")))
  } else {
    project <- list(
      yaml = list(
        project = list(
        )
      ),
      metadata = list(
      )
    )
  }
  project$yaml$project$description <- description

  # save to file
  save_project(
    project = project,
    path = project_yaml_file,
    verbose = verbose,
    force = force
  )

  cli::cli_alert_success("Done")

  # return
  if(return_object) {
    return(project)
  }

}

#' Save a project to a YAML file
#'
#' @param project uno project object
#' @param path path to file to save project
#' @param force force saving of the file, even if it already exists
#' @param verbose verbosity
#'
save_project <- function(
  project,
  path,
  verbose = FALSE,
  force = FALSE
) {
  if(file.exists(path) && !force) {
    cli::cli_abort("Project file exists in this folder. Please specify different project name or folder, or use `force=TRUE` argument.")
  }
  if(verbose) cli::cli_alert_info("Saving project file")
  yaml::write_yaml(project$yaml, path)
}



#' Create a new project object based on a predefined template
#'
#' @param template template for new project. Either a reference to one of the
#' built-in templates (e.g. `default`), or path to a file.
#'
new_project_from_template <- function(
  template = "default"
) {

}
