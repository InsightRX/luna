#' Get data from a model
#'
#' @param id run id, e.g. `run1`
#' @param folder folder containing the model file
#'
#' @export
#'
luna_dataset <- function(
  id,
  folder = "."
) {
  id <- validate_id(id)
  model_file <- file.path(folder, paste0(id, ".mod"))
  if(!file.exists(model_file)) {
    cli::cli_abort("Model file not found. Please check the model file and run folder.")
  }
  dataset <- pharmr::read_model(
    path = model_file
  )$dataset
  if(!is.null(dataset)) {
    return(dataset)
  } else {
    cli::cli_abort("Data file not found. Please check the model file and run folder.")
  }
}
