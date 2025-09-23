#' Run simulations
#'
#' @inheritParams run_nlme
#'
#' @param method either `full` or `sample`, meaning either use the input dataset
#' used to fit the model as-is, or sample subjects from the input dataset. When
#' sampling, the `n_subjects` argument determines the number of subjects to
#' sample, wihch can be either lower or higher than the actual number in the
#' input dataset. Sampling will be done with replacement. Both the `regimen`
#' and `covariates` in the input dataset can be overriden by these respective
#' arguments, see documentation for these arguments.
#' @param regimen if specified, will replace the regimens for each subject with
#' a custom regimen, specified using arguments `dose`, `interval`, and `route`
#' (and `t_inf` / `rate` for infusions).
#' E.g. `regimen = list(dose = 500, interval = 12, route = "oral")`.
#' @param covariates if specified, will replace subjects with subjects specified
#' in a data.frame. In the data.frame, the column names should correspond
#' exactly to any covariates included in the model. An `ID` column is required,
#' and for time-varying covariates, a `TIME` column is also required (otherwise
#' it will be assumed covariates are not changing over time).
#' @param t_obs a vector of observations times. If specified, will override
#' the observations in each subject in the input dataset.
#' @param n_subjects number of subjects to simulate
#' @param n_iterations number of iterations of the entire simulation to
#' perform. The dataset for the simulation will stay the same between each
#' iterations.
#'
#' @returns data.frame with simulation results
#'
#' @export
run_sim <- function(
    fit = NULL,
    data = NULL,
    model = NULL,
    id = "sim1",
    force = FALSE,
    t_obs = NULL,
    dictionary = list(
      ID = "ID",
      DV = "DV",
      EVID = "EVID",
      AMT = "AMT",
      RATE = "RATE",
      CMT = "CMT",
      MDV = "MDV"
    ),
    method = c("full", "sample"),
    regimen = NULL,
    covariates = NULL,
    tool = c("auto", "nonmem", "nlmixr2"),
    n_subjects = NULL,
    n_iterations = 1,
    include_in_output = NULL,
    verbose = TRUE
) {

  ## parse arguments
  if(is.null(fit) && is.null(model)) {
    cli::cli_abort("For simulations we need either a `fit` object, or a `model` file (with updated estimates)")
  }
  if(is.null(model)) {
    model <- attr(fit, "model")
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
  method <- match.arg(method)
  if(method == "sample" && (is.null(n_subjects) || n_subjects == 0)) {
    cli::cli_alert_abort("Sampling method requested, but no `n_subjects` specified.")
  }

  ## Prepare data
  input_data <- model$dataset
  if(method == "full") {
    if(verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- input_data
  } else { ## sampling
    if(verbose) cli::cli_alert_info("Preparing sampled dataset for simulation")
    ids <- unique(input_data[[dictionary$ID]])
    random_sample <- sample(ids, n_subjects, replace = TRUE)
    sim_data <- lapply(seq_along(random_sample), function(i) {
      input_data |>
        dplyr::filter(ID == random_sample[i]) |>
        dplyr::mutate(OLDID = random_sample[i]) |>
        dplyr::mutate(ID = i)
    }) %>%
      dplyr::bind_rows()

    if(!is.null(regimen)) {
      if(verbose) cli::cli_alert_info("Creating new regimens for subjects in simulation")
      cli::cli_abort("Sorry, not implemented yet.")
    }
    if(!is.null(covariates)) {
      if(verbose) cli::cli_alert_info("Updating covariates for subjects in simulation")
      cli::cli_abort("Sorry, not implemented yet.")
    }
  }

  ## Set simulation, and set sim dataset:
  if(verbose) cli::cli_alert_info("Changing model to simution model")
  sim_model <- model |>
    pharmr::set_simulation() |>
    pharmr::set_dataset(sim_data)

  ## Run simulation
  if(verbose) cli::cli_alert_info("Running simulation")
  results <- run_nlme(
    model = model,
    id = id,
    force = force,
    verbose = FALSE
  )

  ## grab table, return
  if(verbose) cli::cli_alert_info("Exporting simulation results")
  tab <- attr(results, "tables")
}
