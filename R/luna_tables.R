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
    if(is_luna_cache_available(abort = FALSE)) {
      folder <- .luna_cache$get("project")$metadata$folder
    } else {
      folder <- "."
    }
  }
  model_file <- find_file_with_fallback(
    folder,
    filename = file.path(id, paste0("run", ".mod")),
    fallback = paste0(id, ".mod"),
    verbose = FALSE
  )
  if(! file.exists(model_file)) {
    cli::cli_abort("Model file not found in run folder")
  }
  model <- pharmr::read_model(model_file)
  get_tables_from_fit(
    model,
    path = file.path(folder, id)
  )
}
