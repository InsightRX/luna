#' Create a folder for a run
#'
#' @inheritParams run_nlme
#'
create_run_folder <- function(
  id,
  path,
  force = FALSE,
  verbose = TRUE
) {
  if(is.null(id)) {
    id <- paste0("run", get_new_run_number(path))
  }
  fit_folder <- file.path(path, id)
  if(dir.exists(fit_folder)) {
    if(force) {
      if(verbose) cli::cli_alert_warning("Existing results found, removing")
      files <- dir(fit_folder, all.files = TRUE)
      for(f in files) {
        if(!stringr::str_detect(f, "^\\.")) {
          unlink(file.path(fit_folder, f))
        }
      }
    } else {
      cli::cli_abort(paste0("Run folder (", fit_folder, ") exists. Use `force` to overwrite."))
    }
  } else {
    dir.create(fit_folder)
  }
  fit_folder
}
