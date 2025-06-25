#' Edit luna config file in editor, e.g. RStudio
#'
#' @param verbose verbose output
#'
#' @export
#'
luna_edit_project <- function(
  verbose = TRUE
) {
  ## Get cache and config
  is_luna_cache_available(abort = TRUE)
  config <- get_luna_config()
  name <- .luna_cache$get("project")$metadata$name
  folder <- .luna_cache$get("project")$metadata$folder
  f <- file.path(folder, paste0(name, ".yaml"))
  if(file.exists(f)) {
    if(verbose)
      cli::cli_alert_info("Opening file {f} in editor.")
    file.edit(f)
  } else {
    cli::cli_abort("Cannot find file project YAML.")
  }
}
