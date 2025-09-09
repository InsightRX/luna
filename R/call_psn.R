#' Call PsN
#'
#' @inheritParams call_nmfe
#' @param options a vector of arguments to pass to the PsN tool, e.g.
#' `c("--samples=100", "--dir="test")`
#'
#' @export
#'
call_psn <- function(
  model_file,
  output_file,
  path,
  options = c(),
  tool = c(
    "execute", "vpc", "bootstrap", "sir", "proseval", "update_inits",
    "cdd"
  ),
  console = TRUE,
  verbose = TRUE
) {

  tool <- match.arg(tool)

  # Transform folder path to absolute path
  path <- normalizePath(path, mustWork = TRUE)

  if(verbose)
    cli::cli_alert_info(paste0("Starting PsN {tool} run in ", path))

  ## Output to console or to file?
  if(console) {
    stdout <- ""
    stderr <- ""
  } else {
    stdout <- file.path(path, "stdout")
    stderr <- file.path(path, "stderr")
  }

  psn_args <- parse_psn_args(options)
  if(verbose) {
    cli::cli_alert_info("Running: {tool} {model_file} {psn_args}")
  }
  withr::with_dir(path, {
    suppressWarnings(
      res <- system2(
        command = tool,
        args = paste(basename(model_file), psn_args),
        wait = TRUE,
        stdout = stdout,
        stderr = stderr
      )
    )
  })
  cli::cli_process_done()
  if(length(res) == 1 && is.numeric(res)) {
    if(res == 127) {
      cli::cli_abort("PsN {tool} was not found. Make sure PsN is installed in your environment and on the path.")
    } else {
      if(res != 0) {
        cli::cli_abort("A unknown error occurred running PsN {tool}. Error code: {res}.")
      } else {
        cli::cli_alert_success("PsN {tool} done.")
      }
    }
  }
}
