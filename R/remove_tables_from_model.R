#' Remove all $TABLE records from a model
#'
#' @param model pharmpy model object
#'
#' @export
#'
remove_tables_from_model <- function(
  model
) {
  tool <- get_tool_from_model(model)
  if(tool == "nonmem") {
    data <- model$dataset
    filename <- tempfile(pattern = "temp_dataset_", fileext = ".csv")
    code_without_tables <- remove_table_sections(model$code)
    write.csv(data, filename, quote=F, row.names=F) # Pharmpy pharmr::set_dataset is unstable, use this trick for now.
    model <- pharmr::read_model_from_string(
      code = code_without_tables
    )
  } else {
    ## Removing tables can only be done for NONMEM datasets
  }
  return(model)
}

#' Function to remove all $TABLE sections
#'
remove_table_sections <- function(text) {
  pattern <- "\\$TABLE[^$]+"
  result <- gsub(pattern, "", text, perl = TRUE)

  # Handle case where $TABLE is the last section
  result <- gsub("\\$TABLE.*$", "", result, perl = TRUE)

  # Clean up any extra newlines that might be left
  result <- gsub("\n{3,}", "\n\n", result)

  # Trim any trailing whitespace
  result <- trimws(result, which = "right")
  return(result)
}
