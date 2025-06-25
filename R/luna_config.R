#' Edit luna config file in editor, e.g. RStudio
#'
#' @param verbose verbose output
#'
#' @export
#'
luna_config <- function(
    verbose = TRUE
) {
  f <- file.path("~", ".config", "luna", "config.yaml")
  if(file.exists(f)) {
    if(verbose)
      cli::cli_alert_info("Opening file {f} in editor.")
    file.edit(f)
  } else {
    cli::cli_abort("Cannot find file {f}.")
  }
}
