#' Read tables for a model and parse into an Xpose database
#'
#' @param id run id
#' @param folder folder
#' @param verbose verbose output?
#'
#' @export
#'
luna_xpose <- function(
  id,
  folder = NULL,
  verbose = TRUE
) {
  if(!rlang::is_installed("xpose")) {
    cli::cli_abort("The `xpose` package is not installed. Please run `install.packages('xpose')`")
  } else {
    if(verbose)
      cli::cli_alert_info("xpose package detected")
  }
  id <- validate_id(id)
  if(is.null(folder)) {
    if(is_luna_cache_available(abort = FALSE)) {
      folder <- .luna_cache$get("project")$metadata$folder
    } else {
      folder <- "."
    }
  }
  output_file <- find_file_with_fallback(
    folder,
    filename = file.path(id, paste0("run", ".lst")),
    fallback = paste0(id, ".lst")
  )
  xpdb <- xpose::xpose_data(file = output_file)
  xpdb
}
