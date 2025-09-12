#' Updates PK parameter tables (patab)
#'
#' E.g. useful to call after pharmr::add_peripheral_compartment() to update
#' the $TABLE with parameter estimates
#'
#' @inheritParams run_nlme
#'
#' @param ... passed to add_default_output_tables()
#'
#' @export
update_pk_tables <- function(model, ...) {

  # figure out which pk parameters
  pk_params <- luna:::get_defined_pk_parameters(model)
  tables <- luna::get_tables_in_model_code(model$code)
  tables <- c("patab", "sdtab")
  data <- model$dataset

  ## remove any table starting with "patab"
  model <- luna::remove_tables_from_model(model, file = "patab")

  ## Re-add dataset
  new_model <- model |>
    pharmr::set_dataset(data)

  ## add back parameter table
  new_model <- luna::add_default_output_tables(
    model = new_model,
    iiv = "all",
    tables = "parameters",
    verbose = FALSE,
    remove_existing = FALSE,
    ...
  )

  ## return new model object
  new_model
}
