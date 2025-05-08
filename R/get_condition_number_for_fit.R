#' Calculate the condition number for a model fit object
#' Performs some safety checks
#'
#' @param fit pharmpy fit object
#'
#' @export
get_condition_number_for_fit <- function(
    fit
) {
  if(is.null(fit$correlation_matrix)) {
    cli::cli_alert_warning("Correlation matrix not available, cannot calculate condition number for fit.")
    return(NA)
  }
  mat <- as.matrix(fit$correlation_matrix)
  if(!inherits(mat, "matrix") || diff(dim(mat)) != 0) {
    cli::cli_abort("Needs a square matrix to calculate condition number.")
  }
  calc_condition_number(mat)
}

#' Calculate the condition number given a matrix
#'
calc_condition_number <- function(mat) {
  e <- eigen(mat)$values
  max(e) / min(e)
}
