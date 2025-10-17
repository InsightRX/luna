#' Get ADVAN number for model
#'
#' @inheritParams run_nlme
#'
#' @returns integer (advan number)
#'
#' @export
#'
get_advan <- function(model) {
  tmp <- model$internals$control_stream$records
  idx <- lapply(seq_along(tmp), function(i) {
    if(inherits(tmp[[i]], "pharmpy.model.external.nonmem.records.subroutine_record.SubroutineRecord")) {
      return(i)
    }
  }) |>
    unlist()
  if(length(idx) >= 1) {
    subroutine <- tmp[[idx[1]]]
  } else {
    return(invisible())
  }
  as.integer(gsub("ADVAN", "", subroutine$advan))
}

#' Get observation compartment number from model
#'
#' @details For ADVAN1-4/11-12 this is easy, for other ADVANs we have to make some
#' assumptions based on whether scaling parameters have already been defined
#' for the model. Logic is as follows:
#'
#' - if S1 is defined and not S2, assume it's 1.
#' - if S2 is defined and not S1, assume it's 2
#' - if both are defined, assume it's 2 but show a warning
#' - if none are defined, assume it's 2 but show a warning
#'
#' @inheritParams get_advan
#'
#' @returns single integer value
#'
#' @export
#'
get_obs_compartment <- function(model) {
  advan <- get_advan(model)
  if(advan %in% c(1, 3, 11)) { # iv
    return(1)
  } else if(advan %in% c(2, 4, 12)) { # with absorption
    return(2)
  } else {
    s1 <- model$statements$find_assignment("S1")
    s2 <- model$statements$find_assignment("S2")
    ode_size <- get_ode_size(model)
    if(!is.null(s1)) {
      if(is.null(s2)) {
        return(1)
      } else {
        comp <- ifelse(ode_size <= 1, 1, 2)
        cli::cli_warn("Scaling parameters S1 and S2 are both defined, and could not determine observation compartment from ADVAN: assuming observation compartment is {ode_size}.")
        return(comp)
      }
    } else {
      if(!is.null(s2)) {
        return(2)
      } else {
        comp <- ifelse(ode_size <= 1, 1, 2)
        cli::cli_warn("No scaling parameters defined yet in model and could not determine observation compartment from ADVAN: assuming observation compartment is {comp}.")
        return(comp)
      }
    }
  }
}

#' Get size of ODE system in $DES
#'
#' @inheritParams get_advan
#'
#' @returns single integer value
#'
#' @export
#'
get_ode_size <- function(model) {
  advan <- get_advan(model)
  if(advan %in% c(1,2,3,4,5,11,12)) {
    return(0)
  }
  length(model$statements$ode_system$compartment_names)
}
