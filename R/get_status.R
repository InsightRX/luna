#' Get the status of a run
#'
#' @param id id of run to get status for
#' @param folder project folder to look in
#'
get_status <- function(id, folder = ".") {
  status <- "not run"
  if(file.exists(file.path(folder, paste0(id, ".lst")))) {
    status <- "finished"
  }
  status
}
