#' Compare basic run info for 2 or more model runs
#'
#' @param ... id of runs to compare. Can be specified either
#' as a vector, or as separate arguments.
#' @param folder folder to look in
#' @param return_object return object?
#'
#' @export
#'
luna_compare <- function(
  ...,
  folder = ".",
  return_object = FALSE
) {
  runs <- list(...)
  if(length(runs) == 1 && length(runs[[1]]) > 1) {
    ## user specified runs as `c("run1", "run2", ...)` instead of specifying as
    ## separate arguments. We just reformat:
    runs <- lapply(unlist(runs), function(x) x )
  }
  model_results <- lapply(runs, function(run) {
    results <- luna_info(run, folder = folder)
    return(results)
  })
  names(model_results) <- runs
  results <- compare_nlme_fit(
    model_results,
    return_object = return_object
  )
}
