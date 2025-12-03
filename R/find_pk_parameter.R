#' Find / match PK parameter based on generic name.
#'
#' E.g. a user may request `pharmr::remove_iiv("V")`, to remove
#' IIV on the central volume. But if in the model the central volume is actually
#' parametrized as `V1` or `V2`, then it will error. When wrapped in
#' `find_pk_parameter` this adds more safety. It will first look if the
#' parameter is used in the model as such. If not found directly, it will
#' attempt other common names for the parameter, depending on the ADVAN number
#'  of the model.
#'
#' @param parameter name of the parameter to find
#' @inheritParams run_nlme
#'
#' @export
#'
find_pk_parameter <- function(parameter, model) {
  ## first try if parameter exist as-is
  model_params <- pharmr::get_pk_parameters(model)
  if(as.character(parameter) %in% model_params) {
    return(parameter)
  }
  ## then, try to find depending on advan
  advan <- get_advan(model)
  if(advan %in% c(1, 3, 11)) {
    map <- list("V" = "V1", "Q" = "QP1", "V2" = "VP1", "V3" = "VP2")
  } else {
    map <- list("V" = "V2", "Q" = "QP1", "V3" = "VP1", "V4" = "VP2")
  }
  if(is.null(map[[parameter]])) {
    cli::cli_warn("Could not find parameter {parameter} in model as {parameter}, nor under different name.")
    return(parameter)
  } else {
    cli::cli_alert_info("Found parameter {parameter} in model as {map[[parameter]]}.")
    return(map[[parameter]])
  }
}
