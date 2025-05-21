#' Read tables for a model and parse into an Xpose database
#'
#' @param id run id
#' @param verbose verbose output?
#'
#' @export
#'
luna_xpose <- function(
  id,
  verbose = TRUE
) {
  runno <- stringr::str_replace(id, "run", "")
  xpdb <- xpose_data(runno = runno)
  xpdb
}
