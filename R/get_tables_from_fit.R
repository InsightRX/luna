#' Read tables created in model run and return as a list of data.frames
#'
#' @param model pharmpy model object
#' @param path path to model execution folder
#'
#' @export
#'
get_tables_from_fit <- function(model, path) {
  table_names <- get_tables_in_model_code(model$code)
  tables <- get_tables_from_folder(
    table_names,
    path
  )
  tables
}

#' Get tables from a folder, by table_names
#'
#' @inheritParams get_tables_from_fit
#' @param table_names file names of tables
#'
get_tables_from_folder <- function(
  table_names,
  path
) {
  tables <- list()
  if(length(table_names) > 0) {
    for(tabnam in table_names) {
      file_name <- file.path(path, tabnam)
      if(file.exists(file_name)) {
        suppressWarnings(
          suppressMessages(
            tables[[tabnam]] <- read_table_nm(file = file.path(path, tabnam))
          )
        )
        if(stringr::str_detect(tabnam, "^patab") && "ID" %in% names(tables[[tabnam]])) {
          tables[[tabnam]] <- tables[[tabnam]] |> # apply FIRSTONLY on patab files
            dplyr::group_by(.data$ID) |>
            dplyr::slice(1)
        }
      }
    }
  }
  tables
}
