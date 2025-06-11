#' Run a pharmpy tool, like bootstrap, or modelsearch
#' 
#' @param model Pharmpy model object, preferably created using 
#' `luna::create_model()`.
#' @param id model id. Optional. If not specified, will generate random modelfit 
#' id. The `id` will be used to create the run folder.
#' @param verbose verbose output?
#' 
#' @return fit object
#' 
#' @export
#' 
run_pharmpy_tool <- function(
  id,
  model,
  results,
  tool = "bootstrap",
  verbose = FALSE,
  ...
) {
  
  run_folder <- file.path(getwd(), id)
  withr::with_dir(run_folder, {
    res <- do.call(
      paste0("run_", tool),
      envir = asNamespace("pharmr"),
      args = list(
        model = model,
        results = results,
        ...
      )
    )
  })

  res
  
}
