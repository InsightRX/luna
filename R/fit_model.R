#' Fit model using NONMEM or nlmixr2
#'
#' Takes a pharmpy-loaded NONMEM model as input, and returns a pharmpy model
#' results object. So essentially this function is a drop-in replacement for the
#' run_modelfit() function in pharmr/pharmpy.
#'
#' @param model pharmpy model object
#' @param data data.frame with data to fit
#' @param tool either `nonmem` or `nlmixr`
#' @param path path to .rds file to save fit results to
#' @param ... passed onto `run_nmfe()` function
#'
#' @export
#'
fit_model <- function(model, data, tool = "nonmem", path, ...) {
  if(! "pharmpy.model.external.nonmem.model.Model" %in% class(model)) {
    cli::cli_abort("Needs a pharmpy model object to run.")
  }

  if(tool == "nonmem") {
    fit <- fit_model_nonmem(model, data, ...)
  } else if (tool == "nlmixr") {
    fit <- fit_model_nlmixr(model, data, ...)
  } else {
    cli::cli_abort(paste("Sorry, fit tool", tool, "not supported"))
  }

  ## save fit object to file
  saveRDS(fit$to_dict(), path)

  ## save model to markdown file
  md_path <- stringr::str_replace(path, "\\.rds", ".md")
  save_model_code(model$model_code, md_path)

  return(fit)
}

#' Fit model using nlmixr2
#'
#' @inheritParams fit_model
#'
fit_model_nlmixr <- function(model, data = NULL, ...) {

  if(is.null(data)) { # get from model reference
    data <- model$dataset
  }

  ## Fit
  fit <- pharmr::run_modelfit(
    model,
    tool = "nlmixr"
  )

  return(fit)
}

#' Fit model using NONMEM
#'
#' @inheritParams fit_model
#'
fit_model_nonmem <- function(model, data = NULL, ...) {

  ## Create temp folder
  folder <- file.path(tempdir(), paste0(model$name, "_", get_datetime_string(), "_", random_string(6)))
  dir.create(folder)
  dataset_filename <- file.path(folder, "nm_data.csv")
  model_filename <- file.path(folder, "run1.mod")

  ## Save dataset to folder
  if(is.null(data)) { # get from model reference
    data <- model$dataset
  }
  nm_save_dataset(data, dataset_filename)

  ## save model to folder
  writeLines(model$model_code, model_filename)

  ## TODO: use Pharmpy to do update datset
  ## rewrite model with updated dataset
  nm_update_dataset(
    model_filename,
    basename(dataset_filename),
    overwrite = TRUE
  )

  ## Run model
  output <- run_nmfe(
    model_filename,
    folder = folder,
    verbose = FALSE
  )

  ## Check for model errors reported from NONMEM
  check_errors_nm_output(output)

  ## Return pharmpy results object, attach output data as well
  res <- pharmr::read_modelfit_results(model_filename)
  attr(res, "sdtab") <- read_table_nm(file.path(folder, "sdtab1"))

  res
}
