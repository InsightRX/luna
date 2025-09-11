#' Wrapper function to add covariates to a pharmpy model
#'
#' @inheritParams create_model
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
