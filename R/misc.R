#' ifelse function but then based on whether value is NULL or not
#'
#' @param value metadata list object
#' @param alternative alternative value
#' @param allow_null can the alternative be NULL?
#'
#' @export
#' @return `value` if non-NULL; `alternative` otherwise
#'
#' @md
ifelse0 <- function (value = NULL, alternative = NULL, allow_null = FALSE) {
  if (is.null(alternative) && !allow_null) {
    stop("No alternative specified")
  }
  if (!is.null(value)) {
    return(value)
  }
  else {
    return(alternative)
  }
}

#' Pluck an inner list element from an outer list, where an element matches
#' (default name is "id") a specific value. Analogue to _.pluck() in
#' JS/underscore
#'
pluck <- function(x, id, el = "id") {
  x[sapply(x, function(x) x[[el]] == id)][[1]]
}

#' Get time to now since a given date, in character
#'
get_time_ago <- function(datetime) {
  t_diff <- lubridate::now() - lubridate::as_datetime(datetime)
  if(is.na(t_diff)) {
    return("")
  } else{
    return(paste(round(t_diff), units(t_diff)))
  }
}

#' Get date/time stamp for last update to file
#'
#' @returns datetime stamp in ISO8601 format. Returns empty string if file
#' does not exist
#'
get_time_last_updated_file <- function(file) {
  dt <- file.mtime(file) |>
    lubridate::as_datetime() |>
    lubridate::format_ISO8601()
  if(is.na(dt)) dt <- ""
  dt
}

#' Get a timestamp for when the last update was made to any file in a folder
#' Will look only 1 level deep.
#'
#' @returns datetime stamp in ISO8601 format. Returns empty string if no
#' files in folder
#'
get_time_last_updated_folder <- function(folder) {
  files <- list.files(folder, full.names = TRUE, recursive = FALSE)
  dt <- lapply(
    files,
    FUN = function(v) file.mtime(v)
  ) |>
    unlist() |>
    max() |>
    lubridate::as_datetime() |>
    lubridate::format_ISO8601()
  if(is.na(dt)) dt <- ""
  dt
}
