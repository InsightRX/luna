#' Run a simulation based on supplied parameters estimates,
#' and combine into proper format for VPC
#'
#' @param fit fit object from `pharmr::run_modelfit()`. Optional, can supply a
#' `model` and `parameters` argument
#' @param model pharmpy model object. Optional, can also only supply just a
#' `fit` object
#' @param parameters list of parameter estimates, e.g. `list(CL = 5, V = 50)`.
#' Optional, can also supply a `fit` object.
#' @param n number of simulation iterations to generate
#' @param keep_columns character vector of column names in original dataset
#' to keep in the output dataset
#' @param verbose verbose output?
#'
#' @export
#'
create_vpc_data <- function(
  fit = NULL,
  model = NULL,
  parameters = NULL,
  keep_columns = c(),
  n = 100,
  verbose = FALSE,
  id = NULL,
  use_pharmpy = TRUE
) {

  ## Make a copy of the model for simulations, and update initial estimates
  tool <- get_tool_from_model(model)
  if(is.null(model)) {
    model <- attr(fit, "model")
    if(is.null(model)) {
      cli::cli_abort("Either a `fit` object with a model attached, or a `model` argument is required.")
    }
  }
  data <- model$dataset
  if(tool != "nonmem") {
    warning("Currently, simulation is not supported by pharmpy for nlmixr-type models. Trying to convert to NONMEM model.")
    ## dataset sometimes gets altered by pharmpy (CMT), make sure this doesn't happen
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

  ## Remove tables and covariance step, add back table with stuff that the VPC needs (ID TIME DV EVID MDV)
  keep <- unique(c("ENC_TIME", keep_columns))
  keep <- keep[keep %in% names(data)]
  sim_model <- sim_model |>
    pharmr::remove_parameter_uncertainty_step() |>
    remove_tables_from_model() |>
    add_table_to_model(
      variables = c("ID", "TIME", "PRED", "DV", "EVID", "MDV", keep),
      firstonly = FALSE,
      file = "sdtab"
    )

  ## Make sure data is clean for modelfit
  sim_model <- clean_modelfit_data(sim_model)

  tmp_path <- file.path(
    tempdir(),
    paste0("simulation_", random_string(5))
  )
  dir.create(tmp_path)
  if(is.null(id)) {
    id <- "tmp"
  }

  ## Run maxeval=0 run to get obs dataset
  if(verbose) cli::cli_alert_info("Running input model evaluation for VPC")
  eval_model <- sim_model |>
    pharmr::set_evaluation_step(idx = 0)
  eval_res <- run_nlme(
    model = eval_model,
    path = tmp_path,
    force = TRUE,
    id = id,
    save_final = FALSE
  )
  obs <- attr(eval_res, "tables")[[1]]

  ## Run the simulation
  if(verbose) cli::cli_alert_info("Running simulation for VPC")
  sim_model <- pharmr::set_simulation(
    sim_model,
    n = n
  )

  sim_data <- run_nlme(
    model = sim_model,
    path = tmp_path,
    force = TRUE,
    id = id,
    save_final = FALSE
  )
  sim <- attr(sim_data, "tables")[[1]]

  ## Parse the output and make ready for vpc::vpc()
  if(verbose) cli::cli_alert_info("Preparing simulated output data for plotting")

  ## Generate a TAD colunmn
  if(is.null(obs$TAD)) {
    obs <- obs |>
      dplyr::group_by(ID) |>
      dplyr::mutate(last_dose_time = if_else(EVID == 1, TIME, NA)) |>
      tidyr::fill(last_dose_time, .direction = "downup") |>
      dplyr::mutate(TAD = TIME - last_dose_time) |>
      dplyr::select(-last_dose_time)
  }

  ## Check if obs and sim match up, and make sure sim has the columns it needs
  len_obs <- nrow(obs)
  len_sim <- nrow(sim)
  if((len_sim %% len_obs) != 0) {
    cli::cli_abort("The simulated dataset length is not a multiple of the length of the original dataset. Please check model and simulation settings.")
  }
  if(use_pharmpy) {
    transfer <- c("ID", "TIME", "PRED", "TAD", "ENC_TIME")
    for(col in transfer) {
      if(!is.null(obs[[col]])) {
        sim[[col]] <- obs[[col]]
      } else {
        cli::cli_alert_warning("Column {col} not found in original dataset.")
      }
    }
  }
  for(col in keep_columns) {
    sim[[col]] <- obs[[col]]
  }

  ## Return
  list(obs = obs, sim = sim)
}
