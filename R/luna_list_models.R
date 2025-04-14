#' Show a listing of finished, running, and to-be-run models
#'
#' @export
#'
luna_list_runs <- function(
  folder = ".",
  project = NULL,
  verbose = FALSE,
  format = "pipe"
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
  yaml <- yaml::read_yaml(yaml_file)

  ## Get the models
  models <- yaml$runs$modelfit

  ## Print a table of models and their descriptions
  if(length(models) > 0) {
    model_table <- data.frame(
      "type" = "modelfit",
      "id" = sapply(models, function(x) x$id),
      "description" = sapply(models, function(x) {
        desc <- x$description
        if (nchar(desc) > 50) {
          paste0(substr(desc, 1, 47), "...")  
        } else {
          desc
        }
      }),
      "status" = sapply(models, function(x) get_status(x$id, folder))
    )
    # Use knitr to create a nice table
    cli::cli_alert_info("Loading models in project {project}:")
    knitr::kable(
      model_table,
      format = format
    )
  } else {
    cli::cli_alert_info("No models found in project.")
  }
} 

#' Get the status of a run
get_status <- function(id, folder = ".") {
  status <- "not run"
  if(file.exists(file.path(folder, paste0(id, ".lst")))) {
    status <- "finished"
  }
  status
}