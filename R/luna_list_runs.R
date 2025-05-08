#' Show a listing of finished, running, and to-be-run models
#'
#' @export
#'
luna_list_runs <- function(
  folder = ".",
  project = NULL,
  arrange = c("type", "status", "id"),
  verbose = FALSE,
  format = "simple"
) {
  yaml_files <- dir(folder, pattern = "\\.yaml$")
  if(length(yaml_files) == 0) {
    cli::cli_abort("No YAML project file(s) found in current working directory.")
  }

  ## Only 1 project file in folder;
  if(length(yaml_files) == 1) {
    project <- yaml_files
  }

  ## No project specified and multiple YAML files:
  if(is.null(project)) {
    cli::cli_abort(
      paste0(
        "Multiple YAML project file(s) found in current working directory, please specify `project`:",
        paste0(yaml_files, collapse = ", ")
      )
    )
  }

  ## Read the YAML file
  yaml_file <- file.path(folder, project)
  if(!file.exists(yaml_file)) {
    cli::cli_abort("YAML project file not found: {yaml_file}")
  }

  ## Read the YAML file
  luna_project <- luna_load_project(yaml_file)

  ## Print info
  print.luna.project(
    luna_project,
    arrange = arrange,
    format = format
  )

}

