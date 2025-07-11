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
    code_without_tables <- remove_table_sections(model$code)
    model <- pharmr::read_model_from_string(
      code = code_without_tables
    )
    if(!is.null(data)) {
      model <- pharmr::set_dataset(model, data)
    }
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
