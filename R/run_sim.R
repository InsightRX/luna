#' Run simulations
#'
#' @inheritParams run_nlme
#'
#' @param regimen if specified, will replace the regimens for each subject with
#' a custom regimen. Can be specified in two ways. The simplest way is to just
#' specify a list with elements `dose`, `interval`, `n`, and
#' `route` (and `t_inf` / `rate` for infusions).
#' E.g. `regimen = list(dose = 500, interval = 12, n = 5, route = "oral")`.
#' Alternatively, regimens can be specified as a data.frame. The data.frame
#' specified all dosing times (`dose`, `time` columns) and `route` and `t_inf` /
#' `rate`. The data.frame may also optionally contain a `regimen` column that
#' specifies a name for the regimen. This can be used to simulate multiple
#' regimens.
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
#' @param update_table should any existing $TABLE records be removed, and a new
#'  `simtab` be created? This is default. If `FALSE`, it will leave $TABLEs as
#' specifed in the model. However, in the return object, only the first table
#' is returned back. If `FALSE`, the `add_pk_variables` argument will be ignored.
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
    update_table = TRUE,
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
  ## make sure we have regimen as a data.frame
  regimen_df <- NULL
  if(!is.null(regimen)) {
    if(inherits(regimen, "data.frame")) {
      regimen_df <- regimen
    } else if (inherits(regimen, "list")) {
      regimen_df <- do.call(create_regimen, args = regimen) |>
        dplyr::mutate(regimen = "regimen 1")
    } else {
      cli::cli_abort("`regimen` needs to be either a data.frame or a list, or NULL.")
    }
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
      cli::cli_warn("`n_subjects` argument can only be used when sampling `covariates`, and will ignored for this simulation.")
    }
    n_subjects <- length(unique(sim_data[[dictionary$ID]]))
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
  if(!is.null(regimen_df)) {
    if(verbose) cli::cli_alert_info("Creating new regimens for subjects in simulation")
    advan <- get_advan(model)
    doses <- create_dosing_records(
      regimen_df,
      sim_data,
      n_subjects,
      dictionary,
      advan
    )
    ## remove old doses and add new
    sim_data <- sim_data |>
      dplyr::filter(EVID != 1) |>
      dplyr::bind_rows(doses) |>
      dplyr::arrange(.regimen, ID, TIME, EVID) |>
      dplyr::group_by(ID) |>
      tidyr::fill(
        dplyr::everything(),
        .direction = "downup"
      )
    if(is.null(t_obs)) {
      ## TODO: could be made somewhat smarter, based on e.g. original dataset or
      t_max <- max(sim_data$TIME) + round(diff(tail(sim_data$TIME, 2)))
      t_obs <- seq(0, t_max, 4)
    }
  } else {
    sim_data[[".regimen"]] <- "original regimens"
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
      dplyr::arrange(.regimen, ID, TIME, EVID) |>
      dplyr::group_by(ID) |>
      tidyr::fill(
        dplyr::everything(),
        .direction = "downup"
      )
  }

  ## get unique regimens / datasets to simulate
  unique_regimens <- unique(sim_data[[".regimen"]])
  comb <- list()

  ## Loop over regimens to simulate
  for(reg_label in unique_regimens) {

    ## grab data for regimen
    sim_data_regimen <- sim_data |>
      dplyr::filter(.regimen == reg_label) |>
      dplyr::select(-.regimen)

    ## Set simulation, and set sim dataset:
    if(verbose) cli::cli_alert_info("Changing model to simulation model")
    sim_model <- model |>
      pharmr::set_simulation(seed = 12345) |>
      pharmr::set_dataset(sim_data_regimen)

    ## Add tables
    if(update_table) {
      if(verbose) cli::cli_alert_info("Updating table record(s)")
      parameter_names <- get_defined_pk_parameters(sim_model)
      variables <- unique(c(variables, parameter_names, names(covariates)))
      sim_model <- sim_model |>
        remove_tables_from_model() |>
        add_table_to_model(variables, file = output_file)
    } else {
      if(verbose) cli::cli_alert_info("Using existing table record(s)")
    }

    ## Run simulation
    if(verbose) cli::cli_alert_info("Running simulation ({reg_label})")
    results <- run_nlme(
      model = sim_model,
      id = id,
      force = TRUE,
      verbose = FALSE
    )

    ## post-processing
    if(update_table) {
      if(add_pk_variables) {
        if(!is.null(regimen_df)) { # regimen needed for calculation of AUCss
          attr(results, "tables")[[output_file]] <- calc_pk_variables(
            data = attr(results, "tables")[[output_file]],
            regimen = regimen_df |>
              dplyr::filter(regimen == reg_label)
          )
        } else {
          attr(results, "tables")[[output_file]] <- calc_pk_variables(
            data = attr(results, "tables")[[output_file]],
            regimen = NULL
          )
        }
      }
    }

    ## grab table, return
    if(verbose) cli::cli_alert_info("Exporting simulation results ({reg_label})")
    comb[[reg_label]] <- attr(results, "tables")

  }

  ## combine back down to single data.frame again
  out <- lapply(unique_regimens, function(reg_label) {
    table_names <- names(comb[[reg_label]])
    simtab <- table_names[1]
    if(!is.null(simtab) && !is.null(comb[[reg_label]][[simtab]])) {
      return(
        comb[[reg_label]][[simtab]] |>
          dplyr::mutate(regimen_label = reg_label)
      )
    } else {
      cli::cli_warn("Simulation for {reg_label} did not output any results.")
      return(data.frame())
    }
  }) |>
    dplyr::bind_rows()

  if(verbose) cli::cli_alert_success("Done")
  out
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

  if(!is.null(data)) {
    ## Find cmax/tmax for each ID
    data <- data |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(CMAX_OBS = max(.data$DV)) |>
      dplyr::mutate(TMAX_OBS = TIME[match(CMAX_OBS[1], DV)][1])

    ## Find Cmin for each ID, for last interval
    tmp_data <- data |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(.dose_id = cumsum(EVID == 1))
    last_obs_dose_id <- tmp_data |>
      dplyr::filter(EVID == 0) |>
      dplyr::pull(.dose_id) |>
      tail(1)
    cmin_data <- tmp_data |>
      dplyr::mutate(.dose_cmin = max(c(1, last_obs_dose_id))) |> # last full interval (before last dose)
      dplyr::filter(.dose_id == .dose_cmin & EVID == 0) |>
      dplyr::summarise(CMIN_OBS = min(DV))
    data <- dplyr::left_join(
      data,
      cmin_data,
      by = "ID"
    )

    ## Add AUC_SS as CL/dose, if we're simulating a specific regimen
    if(!is.null(regimen) && "CL" %in% names(data)) {
      data <- data |>
        dplyr::mutate(AUC_SS = tail(regimen$dose, 1) / .data$CL)
    }
  }

  data
}

#' Create dosing records, given a specified regimen as a data frame with
#' potentially multiple regimens and varying dosing times / doses
#'
create_dosing_records <- function(
    regimen,
    data,
    n_subjects,
    dictionary,
    advan = NULL
) {
  if(!is.null(regimen$regimen)) {
    unq_reg <- unique(regimen$regimen)
  } else {
    regimen$regimen <- "regimen 1"
    unq_reg <- "regimen 1"
  }
  ## logic for picking dosing compartments
  cmt_oral <- 1
  cmt_iv <- 2
  if(!is.null(advan)) {
    if(advan %in% c(1, 3, 11)) {
      cmt_iv <- 1
      if(any(regimen$route) %in% c("oral", "im", "sc")) {
        cli::cli_abort("The model structure does not support oral, im, or sc dosing, only iv.")
      }
    }
  }
  dose <- data.frame(
    ID = 1,
    TIME = regimen$time,
    AMT = regimen$dose,
    EVID = 1,
    MDV = 1,
    DV = 0,
    CMT = 1,
    .regimen = regimen$regimen
  )
  if(is.null(regimen$t_inf)) regimen$t_inf <- 0
  dose$RATE <- 0
  dose$RATE[regimen$t_inf != 0] <- dose$AMT[regimen$t_inf != 0] / regimen$t_inf[regimen$t_inf != 0]
  dose <- dose |>
    dplyr::mutate(CMT = dplyr::case_when(
      regimen$route %in% c("oral", "sc", "im") ~ cmt_oral, # logic for picking dosing cmt
      regimen$route %in% c("iv", "bolus", "infusion") ~ cmt_iv,
      .default = 1
    ))
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
  unq_reg <- unique(data[[".regimen"]])
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
    CMT = cmt,
    RATE = 0
  )
  ## extend single sampling design to multiple subjects
  obs_df <- lapply(1:n_subjects, function(i) {
    obs |>
      dplyr::mutate(ID = i)
  }) |>
    dplyr::bind_rows()
  ## extend to multiple regimens, if needed
  obs_df <- lapply(1:length(unq_reg), function(i) {
    obs_df |>
      dplyr::mutate(.regimen = unq_reg[i])
  }) |>
    dplyr::bind_rows()
  obs_df
}
