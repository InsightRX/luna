#' Run a simulation based on supplied parameters estimates,
#' and combine into proper format for VPC
#'
#' @param fit fit object from `pharmr::run_modelfit()`. Optional, can supply a
#' `model` and `parameters` argument
#' @param model pharmpy model object. Optional, can only supply just a `fit`
#' object
#' @param parameters list of parameter estimates, e.g. `list(CL = 5, V = 50)`.
#' Optional, can also supply a `fit` object.
#' @param n number of simulation iterations to generate
#' @param verbose verbose output?
#'
#' @export
#'
create_vpc_data <- function(
  fit = NULL,
  model = NULL,
  parameters = NULL,
  keep_columns = c("GROUP"),
  n = 100,
  verbose = FALSE
) {

  ## Make a copy of the model for simulations, and update initial estimates
  tool <- get_tool_from_model(model)
  if(tool != "nonmem") {
    warning("Currently, simulation is not supported by pharmpy for nlmixr-type models. Trying to convert to NONMEM model.")
    ## dataset sometimes gets altered by pharmpy (CMT), make sure this doesn't happen
    data <- model$dataset
    model <- pharmr::convert_model(
      model,
      to_format = "nonmem"
    )
    if(!is.null(data)) {
      model <- pharmr::set_dataset(model, data)
    }
  }
  if(!is.null(parameters)) {
    if(verbose) message("Using supplied `parameters` object")
  } else { # try to grab from fit object
    if(!is.null(fit) && !is.null(fit$parameter_estimates)) {
      if(verbose) message("Using parameters from `fit` object")
      parameters <-  as.list(fit$parameter_estimates)
    } else {
      warning("No parameter estimates available, will use initial estimates for VPC!")
    }
  }

  if(is.null(model)) {
    if(verbose) message("Using model from fit object")
    model <- attr(fit, "model")
    if(is.null(model) || !inherits(model, "pharmpy.model.model.Model")) {
      cli::cli_abort("Model is not a pharmpy Model object.")
    }
  }
  if(verbose) message("Updating estimates for simulation model")
  sim_model <- pharmr::set_initial_estimates(
    model,
    inits = parameters
  )

  ## Remove tables, add back table with stuff that the VPC needs (ID TIME DV EVID MDV)
  sim_model <- remove_tables_from_model(sim_model)
  sim_model <- add_table_to_model(
    sim_model,
    variables = c("ID", "TIME", "DV", "EVID", "MDV"),
    firstonly = FALSE,
    file = "sdtab"
  )

  ## Make sure data is clean for modelfit
  sim_model <- clean_modelfit_data(sim_model)

  ## Run the simulation
  if(verbose) message("Running simulations for VPC")
  tmp_path <- file.path(tempdir(), paste0("simulation_", random_string(5)))
  # sim_model <- pharmr::set_simulation(
  #   model,
  #   n = n
  # )
  # sim_model <- pharmr::remove_estimation_step(sim_model, 0)
  sim_model <- pharmr::set_simulation(
    sim_model,
    n = n
  )
  sim_data <- pharmr::run_simulation(
    sim_model,
    path = tmp_path
  )
  if(!inherits(sim_data, "pharmpy.workflows.results.SimulationResults")) {
    cli::cli_abort("Simulation for VPC failed. Please check model and settings.")
  }

  ## Parse the output and make ready for vpc::vpc()
  if(verbose) message("Preparing simulated output data for plotting")
  sim <- sim_data$table
  sim$ID <- model$dataset$ID
  sim$TIME <- model$dataset$TIME
  sim$ENC_TIME <- model$dataset$ENC_TIME
  for(col in keep_columns) {
    sim[[col]] <- model$dataset[[col]]
  }

  ## Return
  list(obs = model$dataset, sim = sim)
}
