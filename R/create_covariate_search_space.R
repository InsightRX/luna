#' Create covariate search space definition for pharmpy `covsearch`
#'
#' See Pharmpy MFL documentation for more info:
#' https://pharmpy.github.io/latest/covsearch.html
#'
#' @param parameters vector of parameter names
#' @param covariates vector of covariate names
#' @param operation parameter-covariate model type (operation)
#' @param explore should the specified `parameters` and `covariates`
#' be used as structural model elements, or as exploration space?
#' @param struct_parameters vector of parameter names for structural model
#' @param struct_covariates vector of covariate names for structural model
#' @param struct_operation parameter-covariate model type (operation)
#' for structural model
#'
#' @export
#'
create_covariate_search_space <- function(
  parameters,
  covariates,
  operation = c("*", "+", "EXP", "POW"),
  explore = TRUE,
  struct_parameters = NULL,
  struct_covariates = NULL,
  struct_operation = c("*", "+", "EXP", "POW")
) {
  operation <- match.arg(operation)
  struct_space <- NULL
  if(!is.null(struct_parameters)) {
    if(is.null(struct_covariates)) {
      cli::cli_abort("Please also specify structural covariates to include.")
    }
    struct_space <- create_covariate_search_space(
      parameters = struct_parameters,
      covariates = struct_covariates,
      operation = struct_operation,
      explore = FALSE
    )
  }
  search_space <- paste0(
    "COVARIATE", ifelse(explore, "?", ""), "([",
    paste0(parameters, collapse=","),
    "], [",
    paste0(covariates, collapse=","),
    "], ",
    operation,
    ")"
  )
  paste0(
    c(struct_space, search_space),
    collapse = "; "
  )
}
