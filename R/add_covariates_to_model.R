#' Wrapper function to add covariates to a pharmpy model
#' 
#' @param model pharmpy model object
#' @param covariates list of parameter-covariate effects, e.g.
#' `list(CL = list(WT = "pow", CRCL = "lin"), V = list(WT = "pow")`
#' Values in list need to match one of the effects allowed by pharmpy.
#' @param data dataset (data.frame). If supplied, will check if the covariate
#' is actually available, and throw a warning (and skip) if not.
#' 
#' @export
add_covariates_to_model <- function(
  model, 
  covariates, 
  data = NULL
) {
  allowed_effects <- c("lin", "pow", "exp", "piece_lin", "cat", "cat2")
  for(par in names(covariates)) {
    for(covt in names(covariates[[par]])) {
      effect <- covariates[[par]][[covt]]
      if(!is.null(data) && !covt %in% names(data)) {
        warning("Covariate `", covt, "` not found in data, skipping.")          
      } else {
        if(!effect %in% allowed_effects) {
          warning("Requested covariate effect type `", effect, "` not recognized, skipping.")
        } else {
          model <- pharmr::add_covariate_effect(
            model = model,
            parameter = par, 
            covariate = covt, 
            effect = effect
          )
        }
      }
    }
  }
  model
}
