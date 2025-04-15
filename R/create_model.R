#' Create model
#' 
#' This is essentially a wrapper around the model-creation and -modification
#' functionality in pharmr/Pharmpy.
#' 
#' @param data data.frame as input to NONMEM / nlmixr.
#' @param route route of administration, either `oral` or `iv`
#' @param lag_time add a lag time, default is `FALSE`
#' @param n_transit_compartments number of transit-compartments for absorption
#' model. Default is `0`.
#' @param bioavailability Add a bioavailability parameter? Default is `FALSE`.
#' Will add using a logit function.
#' @param n_cmt number of elimination and distribution compartments. Default is
#' 1, i.e. no peripheral distributions. 
#' @param elimination elimination type, either `linear` or `michaelis-menten`.
#' @param iiv either `character` or a `list` object. If `character`, should be
#' either "basic" (only CL and V parameters) or "all" (IIV on all parameters). 
#' If specified as a list object, it should contain the IIV magnitude (on SD 
#' scale) for parameters, e.g. `list(CL = 0.2, V = 0.3)`.
#' @param iiv_effect either `character` or `list`. If character, one of 
#' `c("exp", "add", "prop", "log", "re_log")`. If `list`, should specify for
#' each parameter the effect type, e.g. `list(CL = "add", V = "exp")`. Default
#' is `"exp"` for all.
#' @param ruv one of `proportional`, `additive`, or `combined`.
#' @param estimation_method estimation method.
#' @param estimation_options options for estimation method, specified as list,
#'  e.g. `NITER` or `ISAMPLE`.
#' @param uncertainty_method Compute uncertainty for parameter estimations.
#' One of `sandwich` (default), `smat`, `fmat`, `efim`.
#' @param auto_init automatically update initial estimates to reasonable values
#' based on a crude assessment of the PK data. Default is `TRUE`.
#' @param auto_stack_encounters detects if TIME within an individual is 
#' decreasing from one record to another, which NONMEM cannot handle. 
#' If this happens, it will add a reset event (EVID=3) at that time, and 
#' increase the TIME for subsequent events so that NONMEM does not throw an 
#' error. It will increase the time for the next encounter to the maximum 
#' encounter length across all subjects in the dataset (rounded up to 100).
#' If no decreasing TIME is detected, nothing will be done (most common case).
#' This feature is useful e.g. for crossover trials when data on the same 
#' individual ispresent but is included in the dataset as time-after-dose and 
#' not actual time since first overall dose.
#' @param auto_stack_encounters detects if TIME within an individual is 
#' decreasing from one record to another, which NONMEM cannot handle. 
#' If this happens, it will add a reset event (EVID=3) at that time, and 
#' increase the TIME for subsequent events so that NONMEM does not throw an 
#' error. It will increase the time for the next encounter to the maximum 
#' encounter length across all subjects in the dataset (rounded up to 100).
#' If no decreasing TIME is detected, nothing will be done (most common case).
#' This feature is useful e.g. for crossover trials when data on the same 
#' individual ispresent but is included in the dataset as time-after-dose and 
#' not actual time since first overall dose.
#' @param mu_reference MU-reference the model, useful for SAEM estimation
#' method.
#' @param settings additional settings for model creation and model estimation.
#' TBD
#' @param name name of model
#' @param tool output model type, either `nonmem` or `nlmixr`
#' @param verbose verbose output?
#' 
#' @export
#' 
create_model <- function(
    route = c("auto", "oral", "iv"),
    lag_time = FALSE,
    n_transit_compartments = 0,
    bioavailability = FALSE,
    n_cmt = 1,
    elimination = c("linear", "michaelis-menten"),
    iiv = "all",
    iiv_type = "exp",
    ruv = c("additive", "proportional", "combined", "ltbs"),
    covariates = NULL,
    data = NULL,
    name = NULL,
    estimation_method = c("foce", "saem"),
    estimation_options = list(),
    uncertainty_method = c("sandwich", "smat", "rmat", "efim"),
    tool = c("nonmem", "nlmixr", "nlmixr2"),
    auto_init = TRUE,
    auto_stack_encounters = TRUE,
    mu_reference = FALSE,
    settings = list(), # TBD
    verbose = FALSE
) {

  ## Parse arguments
  route <- match.arg(route)
  elimination <- match.arg(elimination)
  ruv <- match.arg(ruv)
  tool <- match.arg(tool)
  estimation_method <- match.arg(estimation_method)
  uncertainty_method <- match.arg(uncertainty_method)

  ## identify tool
  if(tool == "nlmixr2") { # pharmpy identifies "nlmixr2" as "nlmixr"
    tool <- "nlmixr"
  }
  if(verbose) cli::cli_alert_info(paste0("Writing model in ", tool, " format"))
  
  ## Pick route
  if(route == "auto") {
    route <- get_route_from_data(data)
  }
  
  ## Read base model
  if(verbose) cli::cli_alert_info("Reading base model")
  mod <- pharmr::read_model(
    path = system.file(
      paste0("models/nonmem/base_", route, ".mod"), 
      package = "pharmaair"
    )
  )

  ## Absorption
  if(verbose) cli::cli_alert_info("Parsing absorption model")
  if(lag_time) {
    if(route == "iv") {
      cli::clia_alert_warning("IV administration selected, ignoring `lag_time`")
    } else {
      mod <- pharmr::add_lag_time(mod)
    }
  }
  if(isTRUE(bioavailability)) {
    mod <- mod %>%
      pharmr::add_bioavailability(
        add_parameter = TRUE, 
        logit_transform = TRUE
      ) %>%
      pharmr::set_initial_estimates(list(POP_BIO = 0.5))
  }
  if(n_transit_compartments > 0) {
    mod <- pharmr::set_transit_compartments(mod, n = n_transit_compartments)
  }
  
  ## Distribution: add peripheral compartments
  if(n_cmt > 1) {
    if(verbose) cli::cli_alert_info("Adding peripheral compartments")
    for(i in 1:(n_cmt-1)) {
      mod <- pharmr::add_peripheral_compartment(mod)
    }
  }

  ## Elimination
  if(elimination == "michaelis-menten") {
    if(verbose) cli::cli_alert_info("Adding Michaelis-Menten elimination")
    mod <- mod %>% 
      pharmr::set_michaelis_menten_elimination()
  }

  ## set parameter estimates to reasonable values based on data
  if(!is.null(data) && auto_init) {
    if(verbose) cli::cli_alert_info("Setting initial estimates")
    inits <- get_initial_estimates_from_data(data, n_cmt = n_cmt)
    inits <- setNames(inits, paste0("POP_", names(inits)))
    mod <- pharmr::set_initial_estimates(
      model = mod, 
      inits = inits
    )
  }

  ## Add individual variability
  if(verbose) cli::cli_alert_info("Setting IIV")
  mod <- set_iiv(mod, iiv, iiv_type)

  ## Residual error
  if(verbose) cli::cli_alert_info(paste0("Setting error model to: ", ruv))
  mod <- set_residual_error(mod, ruv)
  
  ## Covariates
  if(!is.null(covariates)) {
    if(verbose) cli::cli_alert_info("Adding covariates to model")
    mod <- add_covariates_to_model(
      model = mod, 
      covariates = covariates, 
      data = data
    )
  } else {
    if(verbose) cli::cli_alert_warning("Skipping covariates")
  }

  ## Convert to nlmixr2?
  if(tool == "nlmixr") {
    if(verbose) cli::cli_alert_info("Converting model to nlmixr.")
    mod <- pharmr::convert_model(
      model = mod,
      to_format = "nlmixr"
    )
  }

  ## Estimation method
  steps <- mod$execution_steps$to_dataframe()
  n_steps <- nrow(steps)
  if(! estimation_method %in% steps$method) {
    ## add requested estimation method, with options
    if(tool == "nonmem") {
      if(verbose) cli::cli_alert_info("Setting estimation options")
      tool_options <- get_estimation_options(
        tool, 
        estimation_method, 
        estimation_options
      )
    } else {
      tool_options <- list()
      cli::cli_alert_warning(paste0("Skipping estimation options for ", tool, ", since not supported by Pharmpy. Please set manually"))
    }
    if(verbose) cli::cli_alert_info(paste0("Updating estimation step: ", estimation_method))
    mod <- pharmr::set_estimation_step(
      mod,
      method = estimation_method,
      idx = n_steps - 1,
      interaction = TRUE,
      tool_options = tool_options
    )
  }
  
  ## MU referencing?
  if(mu_reference) {
    mod <- pharmr::mu_reference_model(mod)
  }
  
  ## Parameter uncertainty?
  if(!is.null(uncertainty_method)) {
    if(verbose) cli::cli_alert_info(paste0("Adding parameter uncertainty step: ", uncertainty_method))
    mod <- pharmr::add_parameter_uncertainty_step(
      mod, 
      parameter_uncertainty_method = toupper(uncertainty_method)
    )
  }

  ## Add $TABLE for individual parameter estimates
  if(tool == "nonmem") {
    if(verbose) cli::cli_alert_info("Adding output table for individual parameters")
    ind_parameters <- pharmr::get_individual_parameters(mod)
    mod <- add_table_to_model(
      model = mod, 
      variables = c("ID", ind_parameters), 
      firstonly = TRUE, 
      file = "patab"
    )
  }

  ## Add dataset (needed if we want to add covariates to the model)
  if(!is.null(data)) {
    if(isTRUE(auto_stack_encounters)) {
      data <- stack_encounters(
        data = data,
        verbose = verbose
      )
    }
    if(verbose) cli::cli_alert_info("Updating model dataset with provided dataset")
    mod <- mod %>%
      pharmr::unload_dataset() %>%
      pharmr::set_dataset(
        path_or_df = data, 
        datatype = "nonmem"
      )
  }
  
  ## Set name?
  if(!is.null(name)) {
    mod <- pharmr::set_name(mod, new_name = name)
  }
  
  if(verbose) cli::cli_alert_success("Done")
  
  return(mod)
}

#' Helper function to combine default estimation options with user-specified,
#' and ensure correct format.
#' 
get_estimation_options <- function(tool, estimation_method, estimation_options) {
  tool_options <- estimation_options_defaults[[tool]][[estimation_method]]
  if(!is.null(estimation_options)) {
    tool_options[names(estimation_options)] <- estimation_options
  }
  for(key in names(tool_options)) { # to avoid e.g. `ITER=500.0`
    tool_options[[key]] <- as.character(tool_options[[key]])
  }
  tool_options
}


#' List of default options for estimation method.
#' 
estimation_options_defaults <- list(
  "nonmem" = list(
    "foce" = list(
      MAXEVAL = 2000,
      PRINT = 5,
      POSTHOC = "",
      NOABORT = ""
    ),
    "saem" = list(
      NBURN = 500,
      NITER = 1000,
      ISAMPLE = 2 
    )
  ),
  "nlmixr" = list( ## leave empty for now, pharmpy does not support nlmixr options yet.
    "foce" = list( # https://nlmixr2.org/reference/foceiControl.html
    ),
    "saem" = list(
    )
  )
)

#' Logic to set the residual error model structure for the model
#' 
set_residual_error <- function(mod, ruv) {
  if(ruv == "proportional") {
    mod <- pharmr::set_proportional_error_model(mod)
  } else if (ruv == "additive") {
    mod <- pharmr::set_additive_error_model(mod)
  } else if (ruv == "combined") {
    mod <- pharmr::set_combined_error_model(mod)
  } else if (ruv == "ltbs") {
    mod <- pharmr::set_proportional_error_model(mod, data_trans="log(Y)")
  } else {
    stop("Requested error model structure not recognized.")
  }
  mod
}

#' Get route from data. 
#' If dose and observation events all happen in the same compartment, 
#' then assume IV administration, else oral absorption (or sc, im, etc).
#' 
get_route_from_data <- function(data, default = "iv") {
  if(is.null(data)) {
    return(default)
  }
  dose_cmt <- data %>%
    dplyr::filter(EVID == 1) %>%
    dplyr::pull(CMT) %>%
    unique()
  obs_cmt <- data %>%
    dplyr::filter(EVID == 0) %>%
    dplyr::pull(CMT) %>%
    unique()
  if(length(setdiff(dose_cmt, obs_cmt)) > 0) {
    route <- "oral"
  } else {
    route <- "iv"
  }
  route
}

#' Create a model (old version)
#' 
#' @inheritParams run_modelfit
#' @param n_cmt number of compartments for the population PK model
#' @param route administration route, either `"oral"` or `"iv"` or NULL. If NULL 
#' (default) will read from data (`ROUTE` column) and used route specified for 
#' first dose in dataset.
#' @param absorption one of `"linear"`, `"linear_lag"` (linear with lagtime), or
#'  `"transit"` (transit compartments using the Stirling approximation as 
#'  described in Savic et al. JPKPD 2007). Argument is only required when 
#'  `route` is `"oral"`.
#' @param software Currently only `nlmixr2` supported. 
#' 
#' @returns an R function object for nlmixr2, or a NONMEM model as a 
#' character vector (TODO)
#'  
#' @export
create_model.old <- function(
  n_cmt = 1,
  route = NULL,
  absorption = c("linear", "linear_lag", "transit"),
  software = c("nlmixr2", "nlmixr"),
  data
) {

  ## parse arguments
  absorption <- match.arg(absorption)
  software <- match.arg(software)
  if(is.null(route)) {
    route <- get_route_from_data_column(data$ROUTE)
  }
  if(!isTRUE(route %in% c("iv", "oral"))) {
    stop("Only `iv` and `oral` supported currently as `route`.")
  }

  if(tolower(software) %in% c("nlmixr", "nlmixr2")) {
    extra <- NULL
    if(route == "oral") {
      extra <- dplyr::case_when(
        absorption == "linear_lag" ~ "lag",
        absorption == "transit" ~ "transit",
        .default = NA
      )
      if(is.na(extra)) { extra <- NULL }
    }
    model <- get(
      paste0(c(
        "nlmixr2_pk", 
        paste0(n_cmt, "cmt"), 
        route,
        "linear", 
        extra
        ), collapse = "_"
      )
    )
  }

  model
}
