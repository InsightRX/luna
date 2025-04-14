#' Show a listing of finished, running, and to-be-run models
#'
#' @export
#'
luna_list <- function(
  project = NULL,
  folder = ".",
  verbose = FALSE,
  as_data = FALSE
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


}
