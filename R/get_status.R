#' Get the status of a run
#'
#' @param id id of run to get status for
#' @param folder project folder to look in
#'
get_status <- function(id, folder = ".") {
  status <- "not run"
  tmp <- find_file_with_fallback(
    folder,
    file.path(id, paste0("run", ".lst")),
    fallback = file.path(paste0(id, ".lst")),
    verbose = FALSE,
    abort = FALSE
  )
  if(!is.null(tmp)) {
    status <- "finished"
  }
  status
}
