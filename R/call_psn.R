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
    options = NULL,
    console = FALSE,
    verbose = TRUE
) {

  tool <- match.arg(tool)

  # Transform folder path to absolute path
  path <- normalizePath(path, mustWork = TRUE)

  if(verbose) {
    cli::cli_alert_info("Starting PsN {tool} run in {path}")
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
      args = c(model_file, options),
      wait = TRUE,
      stdout = stdout,
      stderr = stderr
    )
  )
  cli::cli_alert_success("Done")
  handle_system_errors(res, tool)
}
