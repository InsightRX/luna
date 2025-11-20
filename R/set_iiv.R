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
set_iiv <- function(mod, iiv, iiv_type = "exp") {

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
    if(!inherits(iiv, "list")) {
      stop("`iiv` parameter should be a `list` or a `character` object.")
    }

    ## First remove all existing IIV
    ## Then, add univariate IIV (no BLOCKs yet)
    all_params <- get_defined_pk_parameters(mod)
    current <- get_parameters_with_iiv(mod)
    iiv_goal <- names(iiv)[!stringr::str_detect(names(iiv), "~")]
    iiv_corr <- names(iiv)[stringr::str_detect(names(iiv), "~")]
    has_corr <- unique(unlist(stringr::str_split(iiv_corr, "~")))
    to_remove <- setdiff(current, iiv_goal)
    to_reset <- intersect(iiv_goal, current)
    to_add <- setdiff(iiv_goal, current)
    map <- data.frame( # build a map for each parameter, whether it needs to be reset or not
      name = c(to_add, to_reset),
      reset = c(rep(FALSE, length(to_add)), rep(TRUE, length(to_reset)))
    ) |>
      dplyr::mutate(parameter = name) |>
      dplyr::mutate(correlation = name %in% has_corr) |>
      dplyr::arrange(reset, correlation) # make sure to first do the parameters that don't need a reset, to avoid creating DUMMYOMEGA
    for(i in seq_along(map$name)) {
      key <- map$name[i]
      if(key == "V" && (! "V" %in% all_params) && "V1" %in% all_params) {
        map$parameter[i] <- "V1"
      }
      if(key == "Q" && (! "QP1" %in% all_params) && "QP1" %in% all_params) {
        map$parameter[i] <- "QP1"
      }
      names(iiv)[key == names(iiv)] <- map$parameter[i]
    }
    for(i in seq_along(map$parameter)) {
      key <- map$name[i]
      par <- map$parameter[i]
      if(map$reset[match(par, map$parameter)]) {
        mod <- pharmr::remove_iiv(mod, par)
      }
      if(length(mod$statements$find_assignment(par)) > 0) {
        mod <- pharmr::add_iiv(
          model = mod,
          list_of_parameters = par,
          expression = iiv_type_list[[key]],
          initial_estimate = signif(iiv[[par]]^2, 5)
        )
      } else {
        cli::cli_alert_warning(paste0("Parameter declaration for ", key, " not found, cannot add IIV for ", key, "."))
      }
    }

    ## Then, if needed, change relevant $OMEGA to BLOCK
    ## Currently, pharmpy/pharmr does not support setting covariances currently
    ## so we'll write a custom function that just uses regex. It's a hacky solution
    ## but expectation is that pharmr will support this in the future.
    if(length(iiv_corr) > 0) {
      mod <- set_iiv_block(mod, iiv)
    }

  }
  mod
}

set_iiv_block <- function(
  model,
  iiv
) {

  ## make sure we have the IIV object in the same
  ## order as the IIVs in the NONMEM model
  pars <- get_parameters_with_iiv(model)
  iiv_ordered <- list()
  for(par in pars) {
    iiv_ordered[[par]] <- iiv[[par]]
    iiv[[par]] <- NULL
  }
  corr_params <- names(iiv)[grep("~", names(iiv))]
  for(par in corr_params) { # remainder of parameters
    iiv_ordered[[par]] <- iiv[[par]]
  }
  pars_with_corr <- intersect(
    names(iiv_ordered),
    unique(unlist(stringr::str_split(corr_params, "~")))
  )

  ## get omega lines, only the ones with correlations
  code <- stringr::str_split(model$code, "\\n")[[1]]
  omega_idx <- c()
  for(par in pars_with_corr) {
    idx <- grep(paste0("^\\$OMEGA .*? ; IIV_", par), code)
    omega_idx <- c(omega_idx, idx)
  }
  omega_lines <- code[omega_idx]

  ## Create the omega block
  om_block <- get_cov_matrix(
    iiv_ordered,
    nonmem = TRUE,
    limit = 0.001
  )
  omega <- c(
    glue::glue("$OMEGA BLOCK({length(om_block)})"),
    paste(om_block, paste0("; IIV_", pars_with_corr))
  )
  new_code <- c(
    code[1:(min(omega_idx)-1)],
    omega,
    code[(max(omega_idx)+1):length(code)]
  )
  temp <- list(
    code = paste0(new_code, collapse = "\n"),
    dataset = model$dataset,
    datainfo = model$datainfo
  )
  new_model <- create_pharmpy_model_from_list(temp)
  new_model
}

#' Get a character vector with all parameters on which IIV is present
#'
get_parameters_with_iiv <- function(mod) {
  pars <- mod$random_variables$variance_parameters
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
    possible = c("CL", "V", "V1", "V2", "V3", "Q", "Q2", "Q3", "K10", "K12", "K21", "K13", "K31")
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

#' Function to set covariance between parameters in the omega block
#'
#' One caveat is that it will remove any existing covariances, since currently
#' there is no feature in pharmr/pharmpy to extract the covariance info.
#'
#' @inheritParams set_iiv
#' @param covariance character vector specifying the parameters and initial
#' value for the correlation between the respective parameters, e.g.
#' `c("CL~V" = 0.1, "Q~V2" = 0.2)`.
#'
#' @returns Pharmpy model object
#'
#' @export
set_covariance <- function(model, covariance) {
  omegas <- pharmr::get_omegas(model)
  advan <- get_advan(model)
  params <- pharmr::get_pk_parameters(model)
  om_names <- omegas$names |>
    stringr::str_replace_all("IIV_", "")
  om_values <- lapply(omegas$inits, "sqrt") |>
    setNames(om_names)
  ## remove existing covariance entries
  idx_cov <- !grepl("^OMEGA_", om_names)
  om_names <- om_names[idx_cov]
  om_values <- om_values[idx_cov]
  cov_terms <- c()
  for(key in names(covariance)) {
    if(stringr::str_detect(key, "\\~")) {
      om_values[[key]] <- covariance[[key]]
      terms <- stringr::str_split(key, "\\~")[[1]]
      cov_terms_safe <- lapply(terms, find_pk_parameter, model) |>
        as.character()
      names(om_values)[grepl(key, names(om_values))] <- paste(cov_terms_safe, collapse = "~")
      cov_terms <- c(cov_terms, cov_terms_safe)
    }
  }
  om_safe_names <- lapply(om_names, find_pk_parameter, model) |>
    as.character()
  ## Need to re-add parameters that are listed under a different name, e.g. V -> V2
  to_add <- setdiff(om_safe_names, om_names)
  model <- model |>
    pharmr::remove_iiv(to_add) |>
    pharmr::add_iiv(to_add, expression = "exp")
  if(! all(cov_terms %in% om_safe_names)) {
    cli::cli_abort("Cannot add covariance: no IIV is present on one or more of the parameters between which covariance is requested.")
  }
  names(om_values)[1:length(om_safe_names)] <- om_safe_names
  new_model <- set_iiv(
    model,
    iiv = om_values,
    iiv_type = "exp"
  )
  new_model
}
