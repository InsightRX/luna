#' Get the status of a run
#'
#' @param id id of run to get status for
#' @param folder project folder to look in
#'
get_status <- function(id, folder = ".") {
  status <- "not run"
  # NONMEM: check for .lst output file
  tmp <- find_file_with_fallback(
    folder,
    file.path(id, paste0("run", ".lst")),
    fallback = file.path(paste0(id, ".lst")),
    verbose = FALSE,
    abort = FALSE
  )
  if (!is.null(tmp)) {
    status <- "finished"
  }
  # ferx: check for .fitrx result file and convergence
  rds_path <- file.path(folder, paste0(id, ".fitrx"))
  if (file.exists(rds_path)) {
    fit <- tryCatch(ferx::ferx_load_fit(rds_path), error = function(e) NULL)
    if (!is.null(fit) && isTRUE(fit$converged)) {
      status <- "finished"
    } else {
      status <- "failed"
    }
  }
  status
}
