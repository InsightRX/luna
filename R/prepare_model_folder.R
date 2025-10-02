#' Create a folder for running model, with the model and dataset
#'
prepare_run_folder <- function(
  id,
  model,
  path,
  force = FALSE,
  data = NULL,
  auto_stack_encounters = FALSE,
  verbose = TRUE
) {

  ## Create the folder
  fit_folder <- create_run_folder(
    id = id,
    path,
    force = force,
    verbose
  )

  ## Set up other files
  dataset_path <- file.path(fit_folder, "data.csv")
  model_file <- "run.mod"
  output_file <- "run.lst"
  model_path <- file.path(fit_folder, model_file)

  if(verbose) cli::cli_process_start("Checking dataset and copying")
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
  model_code <- change_nonmem_dataset(
    model_code,
    dataset_path
  )
  writeLines(model_code, model_path)
  if(verbose) cli::cli_process_done()

  list(
    model = model,
    model_file = model_file,
    output_file = output_file,
    fit_folder = fit_folder,
    dataset_path = dataset_path
  )
}
