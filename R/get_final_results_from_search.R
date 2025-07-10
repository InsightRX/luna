#' For a Pharmpy grid search, fetch the fit info and attach to object
#' 
#' @param id run id
#' @param results Pharmpy results object from grid search
#' @param tool Pharmpy search tool. If `NULL`, will try to infer from class of
#' results object
#' 
#' @export
#' 
get_final_results_from_search <- function(
  id,
  results,
  tool = NULL,
  verbose = TRUE
) {
  
  if(is.null(tool)) {
    type <- class(results)[1]
    tool <- stringr::str_extract(type, "pharmpy\\.tools\\.([a-z]*)\\.", group = 1)
  }
  if(is.null(tool) || is.na(tool)) {
    cli::cli_abort("Sorry, don't recognize this object as results from a Pharmpy grid search tool.")
  }
  if(is.null(results$final_results)) {
    cli::cli_abort("Sorry, it seems that no final results are available for this grid search.")
  }
  
  ## Get folder name and last fit folder
  folders <- stringr::str_replace_all(
    dir(id, include.dirs = TRUE, pattern = paste0("^", tool, "[0-9].?$")),
    tool,
    ""
  )
  numbers <- as.numeric(folders)
  last_run <- paste0(tool, max(numbers))
  
  ## Try to grab fit info and tables etc.
  fit <- luna::attach_fit_info(
    results$final_results,
    results$final_model,
    fit_folder = file.path(id, last_run, "models", "final")
  )
  
  fit
}