#' Run model with pharmpy
#'
#' @inheritParams call_nmfe
#'
#' @export
#'
call_pharmpy_fit <- function(
    model_file,
    path,
    console = TRUE,
    verbose = TRUE
) {

  if(verbose) {
    cli::cli_alert_info(
      paste0("Starting Pharmpy modelfit in ", path)
    )
  }

  model_path <- file.path(path, model_file)
  model <- pharmr::read_model(model_path)
  withr::with_dir(path, {
    tmp <- pharmr::fit(
      model
    )
  })

  ## Copy all results from temp folder back into main folder
  run_folder <- file.path(path, "modelfit1", "models", "run")
  files <- dir(run_folder)
  for(f in files) {
    f_new <- stringr::str_replace(f, "model\\.", "run.")
    file.copy(
      file.path(run_folder, f),
      file.path(path, f_new)
    )
  }
  unlink(file.path(path, "modelfit1"), recursive = TRUE)
}
