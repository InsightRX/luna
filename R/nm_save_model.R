#' Write a NONMEM model object to file
#'
#' @param model NONMEM model object (imported with `read_nm()`)
#' @param modelfile NONMEM model filename
#' @param overwrite overwrite model file if exists?
#'
#' @export
nm_save_model <- function(
    model = NULL,
    modelfile = NULL,
    overwrite = FALSE) {
  if(is.null(modelfile)) cli::cli_abort("Please specify an output NONMEM modelfile.")
  if(is.null(model)) cli::cli_abort("Please specify an imported NONMEM model (imported with `read_nm`).")
  if(file.exists(modelfile) && !overwrite) {
    cli::cli_abort("Sorry, the output NONMEM file already exists and `overwrite` is set to FALSE.")
  }
  if(! "NONMEM" %in% class(model)) {
    cli::cli_abort("Sorry, this object does not seem to be a valid NONMEM model object. Please import NONMEM files using `nm_read()`")
  }
  # rearrange to make sure record order is correct
  header <- c("PROBLEM", "INPUT", "DATA", "ABBR")
  model <- model[c(header, names(model)[!names(model) %in% header])]
  # write to file
  writeLines(unlist(model), con = modelfile)
}
