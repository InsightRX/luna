#' Call PsN
#'
#' @inheritParams call_nmfe
#'
#' @export
#'
call_psn <- function(
    model_file,
    output_file,
    path,
    tool = c("execute", "vpc", "bootstrap", "sir", "proseval"),
    console = FALSE,
    verbose = TRUE
) {

  tool <- match.arg(tool)

  # Transform folder path to absolute path
  path <- normalizePath(path, mustWork = TRUE)

  if(verbose) {
    cli::cli_process_start(
      paste0("Starting PsN {tool} run in ", path),
      on_exit = "failed"
    )
  }

  ## Output to console or to file?
  if(console) {
    stdout <- ""
    stderr <- ""
  } else {
    stdout <- file.path(path, "stdout")
    stderr <- file.path(path, "stderr")
  }
  curr_dir <- getwd()
  on.exit({
    setwd(curr_dir)
  })
  setwd(path)
  suppressWarnings(
    res <- system2(
      command = tool,
      args = model_file,
      wait = TRUE,
      stdout = stdout,
      stderr = stderr
    )
  )
  cli::cli_process_done()
  handle_run_errors(res, tool)
}
