#' List runs in current project
#'
#' @param raw if `TRUE`, will return a tibble with all information
#' this is more useful for exporting to a file rather than viewing
#' in console.
#'
#' @export
#'
luna_list <- function(
  raw = FALSE
) {

  ## Get cache and config
  is_luna_cache_available(abort = TRUE)
  config <- get_luna_config()
  name <- .luna_cache$get("project")$metadata$name
  folder <- .luna_cache$get("project")$metadata$folder

  ## make sure we're up to date
  luna_load_project(
    name = name,
    folder = folder,
    verbose = FALSE
  )

  ## Load table
  project <- .luna_cache$get("project")
  out <- runs_as_table(project, raw = raw)

  ## select columns
  if(!raw) {
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
  }

  out

}
