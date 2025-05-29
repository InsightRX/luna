#' Validate the specified model, ensure it's valid Pharmpy model
#'
validate_model <- function(
  model
) {
  if(inherits(model, "pharmpy.model.model.Model")) {
    tool <- get_tool_from_model(model)
    if(tool != "nonmem") {
      cli::cli_abort("Currently only NONMEM is supported.")
    }
  } else if(inherits(model, "character")) {
    tool <- "nonmem"
    if(file.exists(model)) { ## specified as file?
      model <- pharmr::read_model(path = model)
    } else { ## specified as code?
      model <- pharmr::read_model_from_string(
        code = paste0(model, collapse = "\n")
      )
    }
  } else {
    cli::cli_abort("`model` should either be model code or a pharmpy model object")
  }
  model
}
