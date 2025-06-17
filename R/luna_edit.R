#' Edit model in editor, e.g. RStudio
#'
#' @param id run id, e.g. `run1`. This will be the folder in which the NONMEM
#' model is run.
#' @param folder path to folder containing the model file. Default is current directory.
#'
#' @export
#'
luna_edit <- function(
  id,
  folder = NULL,
  verbose = TRUE
) {
  id <- validate_id(id)
  if(is.null(folder)) {
    if(is_luna_cache_available(abort = FALSE)) {
      folder <- .luna_cache$get("project")$metadata$folder
    } else {
      folder = "."
    }
  }
  model_file <- file.path(folder, paste0(id, ".mod"))
  if(file.exists(model_file)) {
    if(verbose)
      cli::cli_alert_info("Opening file {model_file} in editor.")
    file.edit(model_file)
  } else {
    cli::cli_abort("Cannot find file {model_file}.")
  }
}
