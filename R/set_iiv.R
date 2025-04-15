#' Set inter-individual variability on parameters
#'
#' @param mod pharmpy model object
#' @param iiv what parameters to put IIV on. Can be one of three formats:
#' - character: `all` or `basic`.
#' - character: `c("CL", "V")`. Will assume SD of 0.5 for initial estimate.
#' - list of numeric: e.g. `list(CL = 0.5, V = 0.5)` with SD for initial 
#' estimates.
#' @param iiv_type one of IIV types accepted by pharmr::add_iiv(), i.e.
#' `add`, `prop`, `exp` (default), `log`, or `re_log`.
#' 
set_iiv <- function(mod, iiv, iiv_type) {
  if(inherits(iiv, "character")) {
    pars <- get_defined_pk_parameters(mod)
    if(length(iiv) == 1 && iiv == "all") {
      iiv <- list()
      for(key in pars) iiv[[key]] <- 0.5
    } else if(length(iiv) == 1 && iiv == "basic") {
      iiv <- list(CL = 0.5)
      if("V" %in% pars) iiv$V <- 0.5
      if("V2" %in% pars) iiv$V2 <- 0.5
    } else { # assume user passed a vector of parameter names to put IIV on
      iiv_list <- list()
      for(key in iiv) iiv_list[[key]] <- 0.5
      iiv <- iiv_list
    }
  }
  ## Make sure iiv_type is a list
  if(inherits(iiv_type, "character")) {
    iiv_type_list <- list()
    for(key in names(iiv)) {
      iiv_type_list[[key]] <- iiv_type
    }
  } else {
    iiv_type_list <- iiv_type
  }
  if(!is.null(iiv)) {
    ## First remove all existing IIV
    current <- get_parameters_with_iiv(mod)
    iiv_goal <- names(iiv)
    to_remove <- setdiff(current, iiv_goal)
    to_reset <- intersect(iiv_goal, current)
    to_add <- setdiff(iiv_goal, current)
    map <- data.frame( # build a map for each parameter, whether it needs to be reset or not
      name = c(to_add, to_reset),
      reset = c(rep(FALSE, length(to_add)), rep(TRUE, length(to_reset)))
    ) %>%
      dplyr::arrange(reset) # make sure to first do the parmaeters that don't need a reset,
    # to avoid creating DUMMYOMEGA
    for(key in map$name) {
      if(map$reset[match(key, map$name)]) {
        mod <- pharmr::remove_iiv(mod, key)
      }
      if(length(mod$statements$find_assignment(key)) > 0) {
        mod <- pharmr::add_iiv(
          model = mod, 
          list_of_parameters = key,
          expression = iiv_type_list[[key]],
          initial_estimate = signif(iiv[[key]]^2, 5)
        )
      } else {
        cli::cli_alert_warning(paste0("Parameter declaration for ", key, " not found, cannot add IIV for ", key, "."))
      }
    }
  }
  mod
}


#' Get a character vector with all parameters on which IIV is present
#' 
get_parameters_with_iiv <- function(mod) {
  pars <- unlist(mod$random_variables$parameter_names)
  idx <- grep("IIV_", pars)
  eta_pars <- c()
  if(length(idx) > 0) {
    eta_pars <- pars[idx]
    eta_pars <- gsub("IIV_", "", eta_pars)
  }
  eta_pars
}

#' Get all parameters that are defined (from a predefined vector of possible parameters)
#' 
get_defined_pk_parameters <- function(
    mod,
    possible = c("CL", "V", "V2", "V3", "Q", "Q2", "Q3", "K10", "K12", "K21", "K13", "K31")
) {
  pars <- c()
  statements <- mod$statements$to_dict()$statements
  for(i in seq(statements)) {
    obj <- statements[[i]]
    if(obj$class == "Assignment") {
      symbol <- gsub("(Symbol\\(\\'|\\'\\))", "", obj$symbol)
      if(symbol %in% possible) {
        pars <- c(pars, symbol)
      }
    }
  }
  pars
}
