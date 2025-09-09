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

  ## Run description, notes, etc
  run <- attr(x, "run")
  if(!is.null(run)) {
    data.frame(
      "Run log" = c("Description", "Notes", "Tags"),
      "Value" = c(
        run$description,
        ifelse0(run$notes, ""),
        paste0(ifelse0(run$tags, ""), collapse = ", ")
      )
    ) |>
      dplyr::filter(Value != "") |>
      knitr::kable(row.names = FALSE, format = "simple") |>
      print()
  }

  ## General run info
  info_tab <- create_modelfit_info_table(x)
  print(knitr::kable(info_tab, row.names = FALSE, format = "simple"))

  ## Parameter estimates + uncertainty
  par_tab <- create_modelfit_parameter_table(x)
  print(knitr::kable(par_tab, row.names = FALSE))

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
  if(!is.na(ofv) && !is.null(x$dofv)) {
    sign <- ifelse(x$dofv < 0, "-", ifelse(x$dofv > 0, "+",))
    dofv <- abs(round(x$dofv, 3))
    ref_run <- paste0(" vs ", x$reference_run)
  } else {
    dofv <- ""
    sign <- ""
    ref_run <- ""
  }
  condition_number <- ifelse(!is.null(x$condition_number), signif(x$condition_number, 3), NA)
  tools <- attr(fit, "tools")
  model <- attr(fit, "model")
  est_steps <- model$execution_steps$to_dataframe()
  info_tab <- data.frame(
    c("OFV:", paste0(ofv, " (", sign, dofv, ref_run, ")")),
    c("Condition number:", condition_number),
    c("ETA Shrinkage: ", paste0(paste0(eta_shrinkage$ETA, ": ", eta_shrinkage$value, " %"), collapse=", ")),
    c("Run info:", ""),
    c("- Estimation step(s): ", ifelse0(paste0(est_steps$method, collapse = ", "), "NA")),
    c("- Minimization success:", x$run_info$minimization_successful),
    c("- Covariance step success:", x$run_info$covstep_successful),
    c("- Evaluations: ", x$function_evaluations),
    c("- Termination cause:", ifelse0(x$run_info$termination_cause, "")),
    c("- Warnings:", paste(x$run_info$warnings, collapse = " / ")),
    c("- Sign. digits:", x$run_info$significant_digits),
    c("- Run time:", paste0(x$runtime$estimation, " sec (estimation), ", x$runtime$total, " sec (total)")),
    c("Tool folders:", ifelse0(paste0(tools, collapse = ", "), "None"))
  ) |>
    t()
  colnames(info_tab) <- c("Result", "Value")
  rownames(info_tab) <- NULL

  info_tab
}

#' Create a data.frame with parameter estimates
#'
create_modelfit_parameter_table <- function(fit) {
  x <- attr(fit, "info")
  if(is.null(x$standard_errors)) {
    stdevs <- rep(NA, length(x$parameter_estimates))
  } else {
    stdevs <- as.numeric(x$standard_errors)
  }
  data.frame(
    Parameter = names(x$parameter_estimates),
    Estimate = as.numeric(
      x$parameter_estimates
    ),
    SD = stdevs
  ) |>
    dplyr::mutate(`RSE %` = dplyr::if_else(
      Estimate != 0,
      round(100 * SD / Estimate, 1),
      NA
    )
  ) |>
    dplyr::select(-SD) |>
    dplyr::mutate(
      Estimate = format(
        signif(Estimate, 5),
        trim = FALSE, drop0trailing = TRUE, scientific = FALSE
      )
    )
}
