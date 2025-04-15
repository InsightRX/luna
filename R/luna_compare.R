luna_compare <- function(
  ..., 
  folder = ".",
  return_object = FALSE
) {
  runs <- list(...)
  model_results <- lapply(runs, function(run) {
    results <- luna_info(run, folder = folder, return_object = TRUE)
    return(results)
  })
  names(model_results) <- runs
  results <- compare_nlme_fit(
    model_results,
    return_object = return_object
  )
}
