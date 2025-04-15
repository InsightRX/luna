#' Read tables created in model run and return as a list of data.frames
#' 
#' @param model pharmpy model object
#' @param path path to model execution folder
#'
#' @export
get_tables_from_fit <- function(model, path) {
  tables <- list()
  table_names <- get_tables_in_model_code(model$code)
  if(length(table_names) > 0) {
    for(tabnam in table_names) {
      file_name <- file.path(path, tabnam)
      if(file.exists(file_name)) {
        suppressWarnings(
          suppressMessages(
            tables[[tabnam]] <- vpc::read_table_nm(file = file.path(path, tabnam))
          )
        )
      }
    }
  }
  tables
}
