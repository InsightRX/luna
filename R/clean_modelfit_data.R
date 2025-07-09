#' Clean / check the dataset before passing to model fitting tool
#'
#' @inheritParams luna_run
#' @param try_make_numeric should function try to turn character columns
#' into numeric columns? If `FALSE` will just set all values to 0 (but
#' retain column to avoid issues).
#'
#' @export
#'
clean_modelfit_data <- function(
  model,
  try_make_numeric = TRUE,
  data = NULL
) {

  tool <- ifelse(inherits(model, "pharmpy.model.external.nonmem.model.Model"), "nonmem", "nlmixr")

  if(is.null(data)) {
    data <- model$dataset
  }
  if(any(lapply(data, class) != "character")) {
    for(key in seq(names(data))) {
      if(inherits(data[[key]], "character")) {
        if(try_make_numeric) {
          cli::cli_alert_warning("Detected character column ({key}), trying to convert to numeric.")
          suppressWarnings({
            data[[key]] <- as.numeric(data[[key]])
            if(any(is.na(data[[key]]))) {
              data[[key]][is.na(data[[key]])] <- 0
            }
          })
        } else {
          cli::cli_alert_warning("Detected character column ({key}), setting to 0.")
          data[[key]] <- NULL
        }
      }
    }

    if(tool != "nonmem") { ## nlmixr2 requires lower-case `cmt`
      data <- data |>
        dplyr::rename(cmt = CMT)
    }

    ## Save dataset
    dataset_file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(data, dataset_file, quote=F, row.names=F)

    ## Update dataset in model
    dat <- luna_dataset(11)

    model <- pharmr::set_dataset(
      model,
      path_or_df = dataset_file,
      datatype = "nonmem"
    )
  }
  model
}
