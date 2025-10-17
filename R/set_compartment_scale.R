#' Set scaling for certain compartments, e.g. dose and observation
#' compartments.
#'
#' Currently not available in Pharmpy, this is a workaround function.
#'
#' @inheritParams run_nlme
#' @param compartment compartment number
#' @param expression specification of new scaling, should always contain variable
#' and scale arguments. E.g. `list(variable = "V", "scale" = 1000)`.
#'
#' @returns Pharmpy model
#'
#' @export
set_compartment_scale <- function(
    model,
    compartment = 1,
    expression = list(variable = "V", scale = 1000),
    verbose = TRUE
) {
  Sx <- paste0("S", compartment)
  curr_expr <- get_compartment_scale(model, compartment)
  if(is.null(curr_expr)) {
    cli::cli_alert_info("No scaling specified for compartment {compartment}, adding scale.")
  } else {
    cli::cli_alert_info("Scaling already specified for compartment {compartment}, updating scale.")
  }
  if(! class(expression$scale) %in% c("numeric", "integer")) {
    cli::cli_abort("`expression$scale` should be a numeric value.")
  }
  new_expr <- paste0(Sx, " = ", expression$variable, "/", expression$scale)

  ## Regex find and update scaling in model code
  code <- stringr::str_split(model$code, "\\n")[[1]]
  pattern <- paste0("^S", compartment, " *=")
  idx <- grep(pattern, code)[1]
  code[idx] <- new_expr
  new_code <- paste0(code, collapse = "\n")

  ## Reload model from file
  pharmr::read_model_from_string(new_code)
}

#' Get compartment scale definition
#'
#' Assumes scale is always defined either as a single variable
#' (e.g. `S2 = V2`), or as a variable divided by a factor (e.g.
#' `S2 = V2/1000`. Other expressions will very likely not result
#'  in a correct extraction, or errors.
#'
#' @inheritParams set_compartment_scale
#'
#' @returns a list with elements `variable` and `scale`, e.g.
#'
get_compartment_scale <- function(model, compartment = 2) {
  tmp <- model$statements$find_assignment(paste0("S", compartment))
  if(!is.null(tmp) && inherits(tmp, "pharmpy.model.statements.Assignment")) {
    elements <- stringr::str_split(as.character(tmp$expression), "\\/")[[1]]
    if(is.na(elements[2])) {
      elements[2] <- 1
    }
    return(list(variable = elements[1], scale = elements[2]))
  } else {
    return(invisible())
  }
}
