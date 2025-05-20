#' List runs in current project
#'
luna_list <- function(
  project = NULL,
  return_object = FALSE
) {

  if(is.null(project)) {
    project <- .luna_cache$get("project")
  }

  project |>
    runs_as_table()

}
