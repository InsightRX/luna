#' List runs in current project
#'
#' @param project project name. Default is `NULL`, will use active project
#'
#' @export
#'
luna_list <- function(
  project = NULL
) {

  if(is.null(project)) {
    project <- .luna_cache$get("project")
  }

  out <- project |>
    runs_as_table()

  config <- get_luna_config()
  cols <- config$general$list$columns
  if(!is.null(cols)) {
    sel_cols <- names(cols)[unlist(cols)]
    check_unknown <- setdiff(sel_cols, names(out))
    if(length(check_unknown) > 0) {
      cli::cli_alert_warning("Columns {check_unknown} not available, please check configuration.")
      sel_cols <- intersect(sel_cols, names(out))
    }
    if(length(sel_cols) > 0) {
      out <- out |>
        dplyr::select(!!sel_cols)
    } else {
      cli::cli_warning("No columns selected, please check luna configuration.")
    }
  }

  out

}
