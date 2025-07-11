## for individual parameter estimates
#' Add one or more default output tables to a model,
#' if they don't already exist in the model.
#'
#' @param model Pharmpy model object
#' @param tables character vector of which default tables
#' to add, options are `fit` and `parameters`.
#' @param full_tables For the default tables, should all input columns from be
#' included in the output tables? Default `FALSE`.
#' @param verbose verbose output?
#'
#' @export
#'
add_default_output_tables <- function(
  model,
  tables = c("fit", "parameters"),
  full_tables = FALSE,
  remove_existing = TRUE,
  verbose = TRUE
) {
  default_table_names <- list(
    "parameters" = "patab",
    "fit" = "sdtab"
  )

  existing_tables <- get_tables_in_model_code(model$code)
  ## by default will remove existing tables
  ## If these are not removed, and patab and sdtab are already present,
  ## will not override them
  if(remove_existing) {
    model <- remove_tables_from_model(model)
    existing_tables <- c()
  }

  ## individual parameters, first row only
  if("parameters" %in% tables && !(default_table_names[["parameters"]] %in% existing_tables)) {
    if(verbose) cli::cli_alert_info("Adding output table for individual parameters")
    cols <- pharmr::get_individual_parameters(model)
    if(full_tables) {
      cols <- unique(c(cols, model$datainfo$names))
    }
    model <- add_table_to_model(
      model = model,
      variables = c("ID", cols),
      firstonly = TRUE,
      file = "patab"
    )
  }

  ## goodness of fit, all rows
  if("fit" %in% tables && !(default_table_names[["fit"]] %in% existing_tables)) {
    if(verbose) cli::cli_alert_info("Adding output table for goodness of fit")
    cols <- c("DV", "EVID", "MDV", "PRED", "IPRED", "CWRES", "NPDE")
    if(full_tables) {
      cols <- unique(c(cols, model$datainfo$names))
    }
    model <- add_table_to_model(
      model = model,
      variables = c("ID", "TIME", cols),
      firstonly = FALSE,
      file = "sdtab"
    )
  }

  model
}
