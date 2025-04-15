#' Remove all $TABLE records from a model
#' 
#' @param model pharmpy model object
#' @param variables character vector with variable names
#' @param firstonly add `FIRSTONLY` parameter to $TABLE record
#' @param file path to file, e.g. `sdtab`
#' 
#' @export
#' 
remove_table_from_model <- function(
    model,
    variables,
    firstonly = FALSE,
    file
) {
  tool <- get_tool_from_model(model)
  if(tool == "nonmem") {
    existing_tables <- get_tables_in_model_code(model$code)
    if(file %in% existing_tables) {
      warning("Table file already in a $TABLE record in model.")
      return(model)
    }
    if(is.null(variables) || length(variables) == 0) {
      warning("No variables to add to $TABLE, skipping.")
      return(model)
    }
    table_code <- paste0(
      "\n$TABLE\n",
      paste0(c(" ", variables), collapse = " "),
      ifelse(firstonly, "\n  FIRSTONLY", ""),
      "\n  NOAPPEND NOPRINT",
      "\n  FILE=", file, 
      "\n\n"
    )
    model <- pharmr::read_model_from_string(
      code = paste0(model$code, table_code)
    )
  } else {
    ## Adding tables can only be done for NONMEM datasets
  }
  return(model)
}