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
  data = NULL,
  verbose = TRUE
) {

  tool <- ifelse(inherits(model, "pharmpy.model.external.nonmem.model.Model"), "nonmem", "nlmixr")

  if(is.null(data)) {
    data <- model$dataset
  }
  if(any(lapply(data, class) != "character")) {
    for(key in names(data)) {
      if(inherits(data[[key]], "character")) {
        if(key %in% c("TIME", "DATE") && all(c("TIME", "DATE") %in% names(data))) {
          ## exception for TIME and DATE columns if they appear together,
          ## don't convert to numeric

        } else {
          if(try_make_numeric) {
            if (key == "DV") {
              ## special handling, if DV includes BLQ values such as "<0.01"
              ## then try to convert to numeric
              idx <- stringr::str_detect(data$DV, "\\<")
              if(any(idx)) { ## add an LLOQ column
                if(verbose)
                  cli::cli_alert_warning("Detected `<` in DV column, adding LLOQ column to handle BLOQ data.")
                data <- data |>
                  dplyr::mutate(DV = as.numeric(stringr::str_replace_all(DV, "\\<", ""))) |>
                  dplyr::mutate(LLOQ = dplyr::if_else(idx, DV, 0)) |>
                  dplyr::mutate(DV = dplyr::if_else(idx, 0, DV))
              }
              tmp_dv <- as.numeric(data$DV)
            } else {
              if(verbose)
                cli::cli_alert_warning("Detected character column ({key}), trying to convert to numeric.")
              suppressWarnings({
                data[[key]] <- as.numeric(data[[key]])
              })
            }
          } else {
            if(verbose)
              cli::cli_alert_warning("Detected character column ({key}), setting to 0.")
            data[[key]] <- NULL
          }
        }
      }
      # make sure all NAs are set to 0
      if(any(is.na(data[[key]]))) {
        data[[key]][is.na(data[[key]])] <- 0
      }
    }

    if(tool != "nonmem") { ## nlmixr2 requires lower-case `cmt`
      data <- data |>
        dplyr::rename(cmt = CMT)
    }

    ## Save dataset
    dataset_file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(data, dataset_file, quote = F, row.names = F)

    ## Update dataset in model
    model <- pharmr::set_dataset(
      model,
      path_or_df = dataset_file,
      datatype = "nonmem"
    )
  }
  model
}
