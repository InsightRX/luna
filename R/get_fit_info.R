#' Get fit info from NONMEM run
#'
#' @param fit pharmpy fit object
#' @param path path to run folder
#' @param output_file NONMEM output file, default is `run.lst`
#'
#' @export
get_fit_info <- function(fit, path = NULL, output_file = "run.lst") {
  lst_file <- file.path(path, output_file)
  fit_info <- list(
    ofv = fit$ofv,
    condition_number = get_condition_number_for_fit(fit),
    shrinkage = get_shrinkage_summary(path = lst_file, fit = fit),
    eta_bar = "TODO",
    iterations = length(fit$ofv_iterations),
    function_evaluations = fit$function_evaluations,
    parameter_estimates = fit$parameter_estimates,
    standard_errors = fit$standard_errors,
    relative_standard_errors = fit$relative_standard_errors,
    runtime = list(
      estimation = fit$estimation_runtime,
      total = fit$runtime_total
    ),
    run_info = list(
      minimization_successful = ifelse(fit$minimization_successful, "yes", "no"),
      covstep_successful = ifelse(fit$covstep_successful, "yes", "no"),
      termination_cause = fit$termination_cause,
      warnings = as.character(fit$warnings),
      significant_digits = fit$significant_digits
    )
  )
  class(fit_info) <- c("list", "pharmpy_fit_info")
  fit_info
}

#' Print function that provides basic run information for a pharmpy modelfit
#'
#' @param x pharmpy fit object
#'
#' @export
print.pharmpy.workflows.results.ModelfitResults <- function(x, ...) {

  ## General run info
  info_tab <- create_modelfit_info_table(x)
  print(knitr::kable(info_tab, row.names = FALSE, format = "simple"))

  ## Parameter estimates + uncertainty
  par_tab <- create_modelfit_parameter_table(x)
  print(knitr::kable(par_tab, row.names = FALSE, format = "simple"))

}

#' Create a data.frame with basic model fit info
#'
#' @param fit pharmpy fit object
#'
create_modelfit_info_table <- function(fit) {
  x <- attr(fit, "info")
  eta_shrinkage <- data.frame()
  etas <- gsub("ETA_", "", names(x$shrinkage$eta))
  for(i in seq(etas)) {
    eta_shrinkage <- dplyr::bind_rows(
      eta_shrinkage,
      data.frame(ETA = etas[i], value = signif(x$shrinkage$eta[i], 3))
    )
  }
  ofv <- ifelse(!is.null(x$ofv), round(x$ofv, 3), NA)
  condition_number <- ifelse(!is.null(x$condition_number), signif(x$condition_number, 3), NA)
  info_tab <- data.frame(
    c("OFV:", ofv),
    c("Condition number:", condition_number),
    c("ETA Shrinkage: ", paste0(paste0(eta_shrinkage$ETA, ": ", eta_shrinkage$value, " %"), collapse=", ")),
    c("Run info:", ""),
    c("- Minimization success:", x$run_info$minimization_successful),
    c("- Covariance step success:", x$run_info$covstep_successful),
    c("- Evaluations: ", x$function_evaluations),
    c("- Termination cause:", x$run_info$termination_cause),
    c("- Warnings:", paste(x$run_info$warnings, collapse = " / ")),
    c("- Sign. digits:", x$run_info$significant_digits),
    c("- Run time:", paste0(x$runtime$estimation, " sec (estimation), ", x$runtime$total, " sec (total)"))
  ) |>
    t()
  colnames(info_tab) <- c(paste0("Run: ", attr(fit, "model")$name), "Result")
  rownames(info_tab) <- NULL
  info_tab
}

#' Create a data.frame with parameter estimates
#'
create_modelfit_parameter_table <- function(fit) {
  x <- attr(fit, "info")
  data.frame(
    Parameter = names(x$parameter_estimates),
    Estimate = as.numeric(x$parameter_estimates),
    SD = as.numeric(x$standard_errors)
  ) |>
    dplyr::mutate(`RSE %` = dplyr::if_else(
      Estimate != 0,
      round(100 * SD / Estimate, 1),
      NA
    )
  ) |>
    dplyr::select(-SD)
}
