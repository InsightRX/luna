#' Validate id
#'
#' @param id run id
#'
validate_id <- function(id) {
  if(is.null(id)) {
    cli::cli_abort("Please specify an `id` to read or update metadata for.")
  }
  if(is.numeric(id)) {
    id <- paste0("run", id)
  }
  id
}
