#' Stack encounters when data from multiple encounters is available for the
#' same ID, and TIME is starting at 0 for each encounter.
#'
#' @param data NONMEM input dataset
#' @param gap rounding resolution for next . E.g. if set to `100` and if the
#' maximum encounter length in the data is 168 hours, will start the encounters
#' at t = 0, 200, 400 etc.
#' @param reset_encounters add an EVID=3 event to reset all compartments to 0 before
#' starting the new encounter? Default is `TRUE`.
#' @param time time column, `"TIME"` by default
#' @param verbose verbose output
#'
#' @export
stack_encounters <- function(
  data,
  gap = 100,
  reset_encounters = TRUE,
  time = "TIME",
  verbose = FALSE
) {
  if(time_is_always_increasing(data, time = time)) {
    ## Still add the column ENC_TIME, for safer post-processing
    data$ENC_TIME <- data$TIME
    return(data)
  } else {
    input_columns <- names(data)
    if(verbose) cli::cli_alert_info("Multiple encounters per subject detected, stacking them in TIME, keeping original TIME column as ENC_TIME column.")
    enct_length <- ceiling(max(data[[time]])/gap) * gap
    tmp <- data |>
      dplyr::mutate(TIME_COLUMN = .data[[time]]) |>
      dplyr::mutate(idx = 1:length(TIME_COLUMN)) |>
      dplyr::group_by(ID) |>
      dplyr::mutate(
        ENC_TIME = TIME_COLUMN,
        prv_time = c(0, TIME_COLUMN)[-length(TIME_COLUMN)],
        is_decreasing = c(0, diff(TIME_COLUMN)) < 0) |>
      dplyr::mutate(
        encounter_idx = cumsum(is_decreasing),
        enct_start_time = encounter_idx * enct_length
      )
    if(reset_encounters) {
      evid3_events <- tmp |>
        dplyr::filter(is_decreasing) |>
        dplyr::mutate(TIME_COLUMN = enct_start_time, EVID = 3, MDV = 1, DV = 0, AMT = 0, idx = idx - 0.5) # make sure to squeeze in, and not make other changes to dataset order
      for(key in names(tmp)) { # revert back auto-converted columns to character
        if(inherits(tmp[[key]], "character")) {
          class(evid3_events[[key]]) <- "character"
        }
      }
      comb <- dplyr::bind_rows(
        tmp |>
          dplyr::mutate(TIME_COLUMN = TIME_COLUMN + ifelse(is.na(enct_start_time), 0, enct_start_time)),
        evid3_events
      ) |>
        dplyr::arrange(idx)
    } else {
      comb <- tmp
    }
    return(
      comb |> ## make sure to remove helper columns
        dplyr::mutate(!!time := TIME_COLUMN) |>
        dplyr::select(!!input_columns, ENC_TIME) |>
        as.data.frame()
    )
  }
}

## Function to check if time in NONMEM dataset is always increasing.
time_is_always_increasing <- function(data, time = "TIME") {
  data |>
    dplyr::group_by(ID) |>
    dplyr::mutate(is_increasing = c(0, diff(.data[[time]])) >= 0) |>
    dplyr::pull(is_increasing) |>
    all()
}
