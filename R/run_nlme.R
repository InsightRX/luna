#' Run model in NONMEM
#'
#' Run the model directly using nmfe (not through pharmpy).
#' This is a more reliable way of running NONMEM, and it is now possible to
#' stream stdout and stderr to file or to console, which is useful for
#' inspection of intermediate model fit.
#'
#' The function does take a pharmpy model as input (optionally), and uses
#' pharmpy to read the results from the model fit, and returns a pharmpy
#' `modelfit` object.
#'
#' @param model pharmpy model object or NONMEM model code (character) or path
#' to NONMEM model file.
#' @param data dataset (data.frame). Optional, can also be included in `model`
#' object (if specified as pharmpy model object).
#' @param id run id, e.g. `run1`. This will be the folder in which the NONMEM
#' model is run. If no folder is specified, it will create a folder `run1` in
#' the current working directory, and will increment the run number for each
#' subsequent run.
#' @param path path to nonmem model. If not specified, will assume current
#' working directory.
#' @param method run method, either `pharmpy` dispatch, `nmfe` or `psn`
#' (psn::execute).
#' @param nmfe full path to nmfe file to run NONMEM with, if `method=="nmfe"`.
#' @param console show stderr and stdout in R console? If FALSE, will stream
#' to files `stdout` and `stderr` in fit folder.
#' @param force if run folder (`id`) exists, should existing results be
#' removed before rerunning NONMEM? Default `FALSE`.
#' @param save_fit save fit object. If `TRUE`, will save as <run_id.rds>. Can
#' also specify filename (rds) to save to.
#' @param save_summary save fit summary and parameter estimates to file?
#' Default is `TRUE`. Will use current folder, and save as
#' `fit_summary_<id>.txt` and `fit_parameters_<id>.csv`.
#' @param auto_stack_encounters only invoked if `data` argument supplied, not if
#' a pharmpy model object is supplied without `data`.
#' Detects if TIME within an individual is
#' decreasing from one record to another, which NONMEM cannot handle.
#' If this happens, it will add a reset event (EVID=3) at that time, and
#' increase the TIME for subsequent events so that NONMEM does not throw an
#' error. It will increase the time for the next encounter to the maximum
#' encounter length across all subjects in the dataset (rounded up to 100).
#' If no decreasing TIME is detected, nothing will be done (most common case).
#' This feature is useful e.g. for crossover trials when data on the same
#' individual ispresent but is included in the dataset as time-after-dose and
#' not actual time since first overall dose.
#' @param clean clean up run folder after NONMEM execution?
#' @param verbose verbose output?
#'
#' @export
#'
run_nlme <- function(
  model,
  data = NULL,
  id,
  path = getwd(),
  method = c("nmfe", "pharmpy", "psn"),
  nmfe = get_nmfe_location_for_run(),
  force = FALSE,
  console = TRUE,
  save_fit = TRUE,
  save_summary = TRUE,
  auto_stack_encounters = TRUE,
  clean = TRUE,
  verbose = TRUE
) {

  time_start <- Sys.time()
  model <- validate_model(model)
  method <- match.arg(method)

  ## Set up run folder
  fit_folder <- create_run_folder(id, path, force, verbose)

  ## Set model name
  model <- pharmr::set_name(
    model = model,
    new_name = id
  )

  ## Set up other files
  dataset_path <- file.path(fit_folder, "data.csv")
  model_file <- "run.mod"
  output_file <- "run.lst"
  model_path <- file.path(fit_folder, model_file)

  ## Make sure data is clean for modelfit
  if(verbose) cli::cli_process_start("Checking dataset and copying")

  ## Add dataset (and potentially stack encounters)
  if(!is.null(data)) {
    if(isTRUE(auto_stack_encounters)) {
      data <- stack_encounters(
        data = data,
        verbose = verbose
      )
    }
    if(verbose) cli::cli_alert_info("Updating model dataset with provided dataset")
    model <- model |>
      pharmr::unload_dataset() |>
      pharmr::set_dataset(
        path_or_df = data,
        datatype = "nonmem"
      )
  }
  model <- clean_modelfit_data(model)
  data <- model$dataset

  ## Copy modelfile + dataset
  write.csv(data, file = dataset_path, quote=F, row.names=F)
  model_code <- model$code
  model_code <- change_nonmem_dataset(model_code, dataset_path)
  writeLines(model_code, model_path)
  if(verbose) cli::cli_process_done()

  ## Run NONMEM and direct stdout/stderr
  if(method == "pharmpy") {
    call_pharmpy_fit(
      model_file = model_file,
      path = fit_folder,
      verbose = verbose,
      console = console
    )
  } else if(method ==  "nmfe") {
    call_nmfe(
      model_file = model_file,
      output_file = output_file,
      path = fit_folder,
      nmfe = nmfe,
      console = console,
      verbose = verbose
    )
  } else if(method == "psn") {
    call_psn(
      model_file = model_file,
      output_file = output_file,
      path = fit_folder,
      tool = "execute",
      console = console,
      verbose = verbose
    )
  } else{
    cli::cli_abort("Model run method {method} not recognized.")
  }

  if(clean) {
    if(verbose) cli::cli_alert_info("Cleaning up run folder")
    clean_nonmem_folder(fit_folder)
  }

  ## Read results using Pharmpy and return
  if(verbose) cli::cli_process_start("Parsing results from run")
  fit <- pharmr::read_modelfit_results(
    file.path(fit_folder, model_file)
  )
  if(is.null(fit)) {
    if(verbose) {
      if(!console) {
        cli::cli_alert_danger("Something went wrong with fit. Output shown below.")
        nmfe_output <- get_nmfe_output(path = fit_folder, output_file)
        log_add(
          event = "error",
          action = "modelfit",
          id = id,
          context = nmfe_output
        )
        print_nmfe_output(nmfe_output)
      }
    }
    cli::cli_abort("No results from modelfit, please check run output.")
  }
  if(verbose) cli::cli_process_done()

  ## Attach model object (with dataset) to fit, for traceability or use in post-processing
  attr(fit, "model") <- model

  ## Attach tables to model fit
  if(verbose) cli::cli_process_start("Importing generated tables")
  tables <- get_tables_from_fit(
    model,
    fit_folder
  )
  attr(fit, "tables") <- tables
  if(verbose) cli::cli_process_done()

  ## Generate a summary of fit info
  if(verbose) cli::cli_process_start("Summarizing fit results")
  fit_info <- get_fit_info(
    fit,
    path = fit_folder,
    output_file = output_file
  )
  attr(fit, "info") <- fit_info
  if(verbose) cli::cli_process_done()

  ## save fit object to file
  if(!is.null(save_fit)){
    if(inherits(save_fit, "character")) {
      saveRDS(fit, save_fit)
    } else if(inherits(save_fit, "logical")) {
      if(save_fit) {
        saveRDS(fit, paste0(id, ".rds"))
      }
    }
  }

  ## save fit summary (fit info and parameter estimates) as JSON
  if(save_summary) {
    if(verbose) cli::cli_process_start("Saving fit results to file")
    fit_summ <- create_modelfit_info_table(fit)
    txt_summ <- knitr::kable(fit_summ, row.names = FALSE, format = "simple")
    writeLines(
      txt_summ,
      paste0(id, "_fit_summary.txt")
    )
    par_est <- create_modelfit_parameter_table(fit)
    write.csv(
      par_est,
      paste0(id, "_fit_parameters.csv"),
      quote=F, row.names=F
    )
    if(verbose) cli::cli_process_done()
  }

  time_end <- Sys.time()
  time_all <- round(as.numeric(time_end - time_start), 1)
  if(verbose) cli::cli_alert_success(paste0("Run done (", time_all,"s)."))

  fit

}

#' Get new run number for model fit
#'
#' @param path path to folder in which to create subfolder for run
#'
get_new_run_number <- function(path = getwd()) {
  folders <- stringr::str_replace_all(
    dir(path, include.dirs = TRUE, pattern = "^run[0-9].?$"),
    "run",
    ""
  )
  numbers <- as.numeric(folders)
  if(length(numbers) == 0) {
    new_number <- 1
  } else {
    new_number <- max(numbers) + 1
  }
  new_number
}

#' Change $DATA in NONMEM model code
#'
#' @param code model code, either as single line string, or vector of lines
#' @param path path of new dataset
#'
change_nonmem_dataset <- function(
  code,
  path
) {

  ## TODO: this implementation is not foolproof, but works in cases
  ##       where the dataset path immediately follows $DATA

  # Find the $DATA line
  if(length(code) == 1) {
    lines <- stringr::str_split(code, pattern = "\\n")[[1]]
  } else {
    lines <- code
  }
  data_line_idx <- grep("^\\$DATA", lines)

  if (length(data_line_idx) == 0) {
    cli::cli_abort("No $DATA line found in the model file")
  }

  # Replace the dataset path while preserving any options after it
  current_line <- lines[data_line_idx]
  parts <- strsplit(current_line, "\\s+")[[1]]
  parts[2] <- path
  lines[data_line_idx] <- paste(parts, collapse = " ")

  code <- paste0(lines, collapse = "\n")
  code
}

#' Call nmfe
#'
#' @param model_file model file, e.g. "run.mod"
#' @param output_file output file, e.g. "run.lst"
#' @param path run folder path, e.g. "run1"
#' @param nmfe path to nmfe batch file to run NONMEM
#' @param console show output from nmfe in console? Default `FALSE`
#' @param verbose verbose output?
#'
#' @export
#'
call_nmfe <- function(
  model_file,
  output_file,
  path,
  nmfe = "/opt/NONMEM/nm_current/run/nmfe75",
  console = FALSE,
  verbose = TRUE
) {

  if(! file.exists(nmfe)) {
    cli::cli_abort("NONMEM (nmfe) not found at {nmfe}")
  } else {
    cli::cli_alert_success("NONMEM found at {nmfe}")
  }

  # Transform folder path to absolute path
  path <- normalizePath(path, mustWork = TRUE)

  if(verbose) {
    cli::cli_process_start(
      paste0("Starting NONMEM (nmfe) run in ", path),
      on_exit = "failed"
    )
  }

  ## Output to console or to file?
  if(console) {
    stdout <- ""
    stderr <- ""
  } else {
    stdout <- file.path(path, "stdout")
    stderr <- file.path(path, "stderr")
  }
  curr_dir <- getwd()
  on.exit({
    setwd(curr_dir)
  })
  setwd(path)
  system2(
    command = nmfe,
    args = c(model_file, output_file),
    wait = TRUE,
    stdout = stdout,
    stderr = stderr,
  )
  cli::cli_process_done()
}

#' Get output from NMFE
#'
#' @param path path to folder with NMFE run
#' @param results_file name of output file
#'
get_nmfe_output <- function(path, results_file = "run.lst") {
  out <- list(
    stderr = NULL,
    stdout = NULL
  )
  if(file.exists(file.path(path, "stderr"))) {
    out$stderr <- readLines(file.path(path, "stderr"))
  }
  if(file.exists(file.path(path, "stdout"))) {
    out$stdout <- readLines(file.path(path, "stdout"))
  }
  if(file.exists(file.path(path, results_file))) {
    out$results_file <- readLines(file.path(path, results_file))
  }
  out
}

#' Print nmfe output (stdout and stderr) from a run folder
#'
#' @param nmfe_output output from nmfe command, as list
#'
print_nmfe_output <- function(
  nmfe_output
) {
  if(length(nmfe_output$stderr) > 0) {
    cli::cli_alert_warning("stderr: ")
    cat(paste0(nmfe_output$stderr, collapse = "\n"), "\n\n")
  } else {
    cli::cli_alert_warning("stderr: <empty>")
  }
  if(length(nmfe_output$stdout) > 0) {
    cli::cli_alert_warning("stdout (last 10 lines): ")
    cat(paste0(tail(nmfe_output$stdout, 10), collapse = "\n"), "\n\n")
  } else {
    cli::cli_alert_warning("stdout: <empty>")
  }
  if(length(nmfe_output$results_file) > 0) {
    cli::cli_alert_warning("results file (last 10 lines): ")
    cat(paste0(tail(nmfe_output$results_file, 10), collapse = "\n"), "\n\n")
  } else {
    cli::cli_alert_warning("results_file: <empty>")
  }
}
