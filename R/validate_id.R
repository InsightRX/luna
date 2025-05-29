#' Validate id
#'
#' @param id run id
#'
validate_id <- function(id) {
  if(is.numeric(id)) {
    id <- paste0("run", id)
  }
  id
}
