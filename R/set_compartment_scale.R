#' Set scaling for certain compartments, e.g. dose and observation
#' compartments.
#'
#' Currently not available in Pharmpy, this is a workaround function.
#'
#' @inheritParams run_nlme
#' @param compartment compartment number. If `NULL` will be attempted to
#' infer from ADVAN. If not a default ADVAN is used, will use 1 as default. So
#' for safe use, please always specify the observation compartment to be scaled.
#' @param expression specification of new scaling, should always contain variable
#' and scale arguments. E.g. `list(variable = "V", "scale" = 1000)`.
#' @param update_inits update initial estimates for basic PK parameters? This is
#' likely needed when applying scale, or else it is very likely that the model
#' starts too far off from the maximum likelihood and the fit will not
#' converge properly. `TRUE` by default.
#'
#' @returns Pharmpy model
#'
#' @export
set_compartment_scale <- function(
    model,
    compartment = NULL,
    expression = list(variable = "V", scale = 1000),
    update_inits = TRUE,
    verbose = TRUE
) {
  ## if compartment not specified, then try to guess
  if(is.null(compartment)) {
    advan <- get_advan(model)
    if(advan %in% c(2, 4, 12)) {
      compartment <- 2
    } else {
      compartment <- 1
      if(! advan %in% c(1, 3, 11)) {
        cli::cli_warn("No `compartment` specified to scale, assuming compartment 1 is observation compartment!")
      }
    }
  }

  ## get current scaling for compartment, if present
  Sx <- paste0("S", compartment)
  curr_expr <- get_compartment_scale(model, compartment)
  if(is.null(curr_expr)) {
    cli::cli_alert_info("No scaling specified for compartment {compartment}, adding scale.")
  } else {
    cli::cli_alert_info("Scaling already specified for compartment {compartment}, updating scale.")
  }
  if(! class(expression$scale) %in% c("numeric", "integer")) {
    cli::cli_abort("`expression$scale` should be a numeric value.")
  }
  expr_var <- find_pk_parameter(expression$variable, model)
  new_expr <- paste0(Sx, " = ", expr_var, "/", expression$scale)

  ## Regex find and update scaling in model code
  code <- stringr::str_split(model$code, "\\n")[[1]]
  pattern <- paste0("^S", compartment, " *=")
  idx <- grep(pattern, code)[1]
  code[idx] <- new_expr
  new_code <- paste0(code, collapse = "\n")

  ## Reload model from file
  model <- pharmr::read_model_from_string(new_code)

  ## Update initial estimates for PK parameters
  if(update_inits) {
    model <- scale_initial_estimates_pk(
      model,
      scale = expression$scale
    )
  }

  model
}

#` Scale initial estimates by factor
#'
#' Only applies to PK parameters, not all parameters
#'
#' @inheritParams set_compartment_scale
#'
#' @returns Pharmpy model object
#'
#' @export
#'
scale_initial_estimates_pk <- function(
    model,
    scale
) {
  pars <- model$parameters$to_dataframe()
  pk_params <- rownames(pars)
  all_pk <- c(
    "CL", "V",
    "V1", "V2", "V3", "V4",
    "VP1", "VP2", "VP3",
    "Q", "Q1", "Q2", "Q3", "QP1", "QP2", "QP3"
  )
  par_to_scale <- intersect(
    pk_params,
    c(all_pk, paste0("POP_", all_pk))
  )
  updated_inits <- list()
  for(key in par_to_scale) {
    idx <- match(key, pk_params)
    if(!is.na(idx)) {
      updated_inits[[key]] <- pars$value[idx] * scale
    }
  }
  model <- pharmr::set_initial_estimates(
    model,
    inits = updated_inits
  )
  model
}

#' Get compartment scale definition
#'
#' Assumes scale is always defined either as a single variable
#' (e.g. `S2 = V2`), or as a variable divided by a factor (e.g.
#' `S2 = V2/1000`. Other expressions will very likely not result
#'  in a correct extraction, or errors.
#'
#' @inheritParams set_compartment_scale
#'
#' @returns a list with elements `variable` and `scale`, e.g.
#'
get_compartment_scale <- function(model, compartment = 2) {
  tmp <- model$statements$find_assignment(paste0("S", compartment))
  if(!is.null(tmp) && inherits(tmp, "pharmpy.model.statements.Assignment")) {
    elements <- stringr::str_split(as.character(tmp$expression), "\\/")[[1]]
    if(is.na(elements[2])) {
      elements[2] <- 1
    }
    return(list(variable = elements[1], scale = as.numeric(elements[2])))
  } else {
    return(invisible())
  }
}
