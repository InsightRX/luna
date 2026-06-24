#' Show fit results for a ferx model run
#'
#' Reads the `{id}-fit.rds` file saved by [luna_run()] (when
#' `method = "ferx"`) and prints a formatted parameter summary.
#'
#' @inheritParams luna_run
#'
#' @return The `ferx_fit` result object, invisibly.
#'
#' @export
luna_ferx_info <- function(id, folder = NULL) {
  id <- validate_id(id)
  if (is.null(folder)) {
    folder <- .luna_cache$get("project")$metadata$folder
  }
  folder <- normalizePath(folder, mustWork = TRUE)

  result_file <- file.path(folder, paste0(id, "-fit.rds"))
  if (!file.exists(result_file)) {
    cli::cli_abort(
      c(
        "No ferx results found for run {.val {id}}.",
        "i" = "Run {.fn luna_run} first, or check that {.file {result_file}} exists."
      )
    )
  }

  result <- readRDS(result_file)

  cli::cli_h1("ferx fit: {id}")
  cli::cli_alert_info("Method:    {result$method}")
  cli::cli_alert_info("Converged: {result$converged}")
  cli::cli_alert_info("OFV:       {round(result$ofv, 4)}")
  if (!is.null(result$aic))
    cli::cli_alert_info("AIC:       {round(result$aic, 4)}")
  if (!is.null(result$bic))
    cli::cli_alert_info("BIC:       {round(result$bic, 4)}")
  if (!is.null(result$n_iterations))
    cli::cli_alert_info("Iterations: {result$n_iterations}")
  if (!is.null(result$wall_time_secs))
    cli::cli_alert_info("Wall time:  {round(result$wall_time_secs, 1)} s")

  invisible(result)
}
