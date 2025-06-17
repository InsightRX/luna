#' Get tables from a model / run
#'
#' @inheritParams luna_run
#'
#' @export
#'
luna_tables <- function(
  id,
  folder = NULL,
  verbose = TRUE
) {
  id <- validate_id(id)
  if(verbose)
    cli::cli_alert_info("Looking for output tables for run {id}")
  if(is.null(folder)) {
    is_luna_cache_available(abort = TRUE)
    folder <- .luna_cache$get("project")$metadata$folder
  }
  model_file <- paste0(id, ".mod")
  model <- pharmr::read_model(file.path(folder, model_file))
  get_tables_from_fit(
    model,
    path = folder
  )
}
