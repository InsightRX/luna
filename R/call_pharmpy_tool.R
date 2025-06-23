#' Generic function for running a pharmpy tool, like bootstrap,
#' or modelsearch. A separate function is available for `fit()`
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
call_pharmpy_tool <- function(
  id,
  model,
  results,
  tool,
  verbose = FALSE,
  ...
) {

  if(verbose) {
    cli::cli_process_start(
      paste0("Starting {tool} in ", path),
      on_exit = "failed"
    )
  }

  ## Run fit
  run_folder <- file.path(getwd(), id)
  if(!dir.exists(run_folder))
    dir.create(run_folder)
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
