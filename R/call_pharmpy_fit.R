#' Run model with pharmpy
#'
#' @inheritParams call_nmfe
#'
#' @export
#'
call_pharmpy_fit <- function(
    model_file,
    path,
    clean = TRUE,
    console = TRUE,
    verbose = TRUE
) {

  if(verbose) {
    cli::cli_alert_info(
      paste0("Starting Pharmpy modelfit in ", path)
    )
  }

  ## Clean run folders, if requested
  clean_pharmpy_runfolders(
    id = NULL,
    folder = path,
    tool = "modelfit",
    remove = clean
  )

  ## Read model
  model_paths <- file.path(path, model_file)
  models <- list()
  if(length(model_paths) == 1) {
    models <- pharmr::read_model(model_paths[1])
  } else {
    for(model_path in model_paths) {
      models <- c(models, pharmr::read_model(model_path))
    }
  }

  ## Run model
  withr::with_dir(path, {
    tmp <- pharmr::fit(
      models
    )
  })

  ## Copy all results from modelfit folder back into main folder
  for(p in model_paths) {
    last_pharmpy_runfolder <- tail(get_pharmpy_runfolders(id = NULL, folder = path, tool = "modelfit"), 1)
    run_folder <- file.path(path, last_pharmpy_runfolder, "models", "run")
    files <- dir(run_folder)
    for(f in files) {
      f_new <- stringr::str_replace(f, "model\\.", "run.")
      file.copy(
        file.path(run_folder, f),
        file.path(path, f_new)
      )
    }
  }

}
