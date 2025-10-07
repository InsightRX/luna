#' Run simulations
#'
#' @inheritParams run_nlme
#'
#' @param regimen if specified, will replace the regimens for each subject with
#' a custom regimen, specified using arguments `dose`, `interval`, `n`, and
#' `route` (and `t_inf` / `rate` for infusions).
#' E.g. `regimen = list(dose = 500, interval = 12, n = 5, route = "oral")`.
#' @param covariates if specified, will replace subjects with subjects specified
#' in a data.frame. In the data.frame, the column names should correspond
#' exactly to any covariates included in the model. An `ID` column is required,
#' and for time-varying covariates, a `TIME` column is also required (otherwise
#' it will be assumed covariates are not changing over time).
#' @param t_obs a vector of observations times. If specified, will override
#' the observations in each subject in the input dataset.
#' @param n_subjects number of subjects to simulate, when using sampled data
#' (i.e. requires `covariates` argument)
#' @param n_iterations number of iterations of the entire simulation to
#' perform. The dataset for the simulation will stay the same between each
#' iterations.
#' @param add_pk_variables calculate basic PK variables that can be extracted
#' in post-processing, such as CMAX_OBS, TMAX_OBS, AUC_SS.
#'
#' @returns data.frame with simulation results
#'
#' @export
run_sim <- function(
    fit = NULL,
    data = NULL,
    model = NULL,
    id = get_random_id("sim_"),
    force = FALSE,
    t_obs = NULL,
    dictionary = list(
      ID = "ID",
      DV = "DV",
      EVID = "EVID",
      AMT = "AMT",
      CMT = "CMT",
      MDV = "MDV"
    ),
    regimen = NULL,
    covariates = NULL,
    tool = c("auto", "nonmem", "nlmixr2"),
    n_subjects = NULL,
    n_iterations = 1,
    variables = c("ID", "TIME", "DV", "EVID", "IPRED", "PRED"),
    add_pk_variables = TRUE,
    output_file = "simtab",
    seed = 12345,
    verbose = TRUE
) {

  ## parse arguments
  if(is.null(fit) && is.null(model)) {
    cli::cli_abort("For simulations we need either a `fit` object, or a `model` file (with updated estimates)")
  }
  if(is.null(model)) {
    if(!is.null(attr(fit, "final_model"))) {
      model <- attr(fit, "final_model")
    } else {
      cli::cli_abort("No proper model object available. Need either a `model` object or a `fit` object with a model attached.")
    }
  }
  tool <- match.arg(tool)
  if(tool == "auto") {
    if(inherits(model, "pharmpy.model.external.nonmem.model.Model")) {
      tool <- "nonmem"
    }
  }
  if(tool != "nonmem") {
    cli::cli_abort("Sorry, currently only supporting NONMEM simulations.")
  }

  ## Prepare data
  if(is.null(data)) {
    input_data <- model$dataset
  } else {
    input_data <- data
  }
  if(is.null(covariates)) { # use original dataset
    if(verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- input_data
    if(!is.null(dictionary)) {
      sim_data <- sim_data |>
        dplyr::rename(
          !!!rlang::set_names(
            dictionary,
            names(dictionary)
          )
        )
    }
    if(!is.null(n_subjects)) {
      cli::cli_warn("`n_subjects` can only be used in when sampling `covariates`, and will be ignored for this simulation.")
    }
  } else { ## use provided sampled covariates in `data`
    if(is.null(n_subjects)) {
      cli::cli_abort("For sampling new datasets, need `n_subjects` argument.")
    }
    if(verbose) cli::cli_alert_info("Preparing sampled dataset for simulation")
    ids <- unique(input_data[[dictionary$ID]])
    random_sample <- sample(ids, n_subjects, replace = TRUE)
    sim_data <- lapply(seq_along(random_sample), function(i) {
      input_data |>
        dplyr::filter(ID == random_sample[i]) |>
        dplyr::mutate(ID = i)
    }) %>%
      dplyr::bind_rows()
    if(!is.null(covariates)) {
      if(verbose) cli::cli_alert_info("Updating covariates for subjects in simulation")
      covs_reqd <- unlist(lapply(
        pharmr::get_model_covariates(model),
        function(x) { x$name }
      ))
      if(! all(covs_reqd %in% names(covariates))) {
        missing <- covs_reqd[! covs_reqd %in% names(covariates)]
        cli::cli_abort("Not all required covariates supplied in `covariates` data, missing: {missing}")
      }
      if(! "ID" %in% names(covariates)) {
        covariates$ID <- 1:nrow(covariates)
      }
      if(! "TIME" %in% names(covariates)) {
        covariates$TIME <- 0
      }
      new_covariates <- names(covariates)
      new_covariates <- new_covariates[(! new_covariates %in% c("ID", "TIME")) & new_covariates %in% names(sim_data)]
      sim_data <- sim_data |>
        dplyr::select(- new_covariates) |> ## remove existing covariates
        dplyr::left_join(
          covariates,
          by = join_by(ID == ID, TIME == TIME)
        ) |>
        tidyr::fill(new_covariates, .direction = "downup")
    }
  }
  if(!is.null(regimen)) {
    if(verbose) cli::cli_alert_info("Creating new regimens for subjects in simulation")
    doses <- create_dosing_records(
      regimen,
      sim_data,
      n_subjects,
      dictionary
    )
    ## remove old doses and add new
    sim_data <- sim_data |>
      dplyr::filter(EVID != 1) |>
      dplyr::bind_rows(doses) |>
      dplyr::arrange(ID, TIME, EVID) |>
      dplyr::group_by(ID) |>
      tidyr::fill(
        dplyr::everything(),
        .direction = "downup"
      )
  }
  if(!is.null(t_obs)) {
    if(verbose) cli::cli_alert_info("Creating new observation records for subjects in simulation")
    obs <- create_obs_records(
      sim_data,
      t_obs,
      n_subjects,
      dictionary
    )
    ## remove old obs and add new
    sim_data <- sim_data |>
      dplyr::filter(EVID != 0) |>
      dplyr::bind_rows(obs) |>
      dplyr::arrange(ID, TIME, EVID) |>
      dplyr::group_by(ID) |>
      tidyr::fill(
        dplyr::everything(),
        .direction = "downup"
      )
  }

  ## Set simulation, and set sim dataset:
  if(verbose) cli::cli_alert_info("Changing model to simulation model")
  sim_model <- model |>
    pharmr::set_simulation(seed = 12345) |>
    pharmr::set_dataset(sim_data)

  ## Add tables
  if(verbose) cli::cli_alert_info("Updating table record")
  parameter_names <- get_defined_pk_parameters(sim_model)
  variables <- unique(c(variables, parameter_names, names(covariates)))
  sim_model <- sim_model |>
    remove_tables_from_model() |>
    add_table_to_model(variables, file = output_file)

  ## Run simulation
  if(verbose) cli::cli_alert_info("Running simulation")
  results <- run_nlme(
    model = sim_model,
    id = id,
    force = force,
    verbose = FALSE
  )

  ## post-processing
  if(add_pk_variables) {
    attr(results, "tables")[[output_file]] <- calc_pk_variables(
      data = attr(results, "tables")[[output_file]],
      regimen = regimen
    )
  }

  ## grab table, return
  if(verbose) cli::cli_alert_info("Exporting simulation results")
  attr(results, "tables")
}

#' Calculate some basic PK variables from simulated or observed data
#'
#' @param data data.frame in NONMEM format
#' @inheritParams run_sim
#'
#' @returns data.frame
calc_pk_variables <- function(
    data,
    regimen = NULL,
    dictionary = NULL
) {

  ## Find cmax/tmax for each ID
  if(!is.null(data)) {
    data <- data |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(CMAX_OBS = max(.data$DV)) |>
      dplyr::mutate(TMAX_OBS = TIME[match(CMAX_OBS[1], DV)][1])

    ## Add AUC_SS as CL/dose, if we're simulating a specific regimen
    if(!is.null(regimen) && "CL" %in% names(data)) {
      data <- data |>
        dplyr::mutate(AUC_SS = regimen$dose / .data$CL)
    }
  }

  data
}

#' Create dosing records, given a specified regimen
#'
create_dosing_records <- function(
    regimen,
    data,
    n_subjects,
    dictionary
) {
  required <- c("dose", "interval", "n", "route")
  if(! all(required %in% names(regimen))) {
    cli::cli_abort("Regimen needs to be specified using required variables: {required}")
  }
  ## create a template row
  cmt <- data |>
    dplyr::filter(ID == 1 & EVID == 1) |>
    dplyr::slice(1) |>
    dplyr::pull(CMT)
  if(is.null(cmt)) cmt <- 1
  dose <- data.frame(
    ID = 1,
    TIME = seq(0, (regimen$n-1) * regimen$interval, by = regimen$interval),
    AMT = regimen$dose[1],
    EVID = 1,
    MDV = 1,
    DV = 0,
    CMT = cmt
  )
  if(regimen$route %in% c("iv", "sc")) {
    if(!is.null(regimen$t_inf)) {
      dose$RATE <- dose$AMT / regimen$t_inf
    }
  }
  dose_df <- lapply(1:n_subjects, function(i) {
    dose |>
      dplyr::mutate(ID = i)
  }) |>
    dplyr::bind_rows()
  dose_df
}

#' Create observation records, given a specified t_obs vector
#'
create_obs_records <- function(
    data,
    t_obs,
    n_subjects,
    dictionary
) {
  ## create a template row
  cmt <- data |>
    dplyr::filter(ID == 1 & EVID == 0) |>
    dplyr::slice(1) |>
    dplyr::pull(CMT)
  if(is.null(cmt)) cmt <- 1
  obs <- data.frame(
    ID = 1,
    TIME = t_obs,
    AMT = 0,
    EVID = 0,
    MDV = 0,
    DV = 0,
    CMT = cmt
  )
  obs_df <- lapply(1:n_subjects, function(i) {
    obs |>
      dplyr::mutate(ID = i)
  }) |>
    dplyr::bind_rows()
  obs_df
}
