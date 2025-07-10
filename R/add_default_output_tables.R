## for individual parameter estimates
#' Add one or more default output tables to a model,
#' if they don't already exist in the model.
#' 
#' @param model Pharmpy model object
#' @param tables character vector of which default tables
#' to add, options are `fit` and `parameters`.
#' @param verbose verbose output?
#' 
#' @export
#' 
add_default_output_tables <- function(
  model,
  tables = c("fit", "parameters"),
  verbose = TRUE
) {
  default_table_names <- list(
    "parameters" = "patab",
    "fit" = "sdtab"
  )
  existing_tables <- get_tables_in_model_code(model$code)
  
  ## individual parameters, first row only
  if("parameters" %in% tables && !(default_table_names[["parameters"]] %in% existing_tables)) {
    if(verbose) cli::cli_alert_info("Adding output table for individual parameters")
    ind_parameters <- pharmr::get_individual_parameters(model)
    model <- add_table_to_model(
      model = model,
      variables = c("ID", ind_parameters),
      firstonly = TRUE,
      file = "patab"
    )
  }
  
  ## goodness of fit, all rows
  if("fit" %in% tables && !(default_table_names[["fit"]] %in% existing_tables)) {
    if(verbose) cli::cli_alert_info("Adding output table for goodness of fit")
    gof_vars <- c("DV", "EVID", "MDV", "PRED", "IPRED", "CWRES", "NPDE")
    model <- add_table_to_model(
      model = model,
      variables = c("ID", "TIME", gof_vars),
      firstonly = FALSE,
      file = "sdtab"
    )
  }
  
  model
}
