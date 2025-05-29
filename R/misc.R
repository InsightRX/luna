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

#' Pluck an inner list element from an unnamed outer list, where an element matches
#' (default name is "id") a specific value. Analogue to _.pluck() in
#' JS/underscore
#'
pluck_entry <- function(x, id, el = "id") {
  x_filtered <- x[sapply(x, function(x) x[[el]] == id)]
  if(length(x_filtered) != 0) {
    x_filtered[[1]]
  }
}

#' The reverse of pluck_entry, insert_entry() inserts
#' an entry into an unnamded outer list, based on an element
#' in the inner list.
#'
insert_entry <- function(x, id, entry, el = "id") {
  for(i in seq(x)) {
    obj <- x[[i]]
    if(obj[[el]] == id) {
      obj <- entry
      x[[i]] <- obj
    }
  }
  x
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

#' Find a file(s) from a model run with a potential fallback
#'
#' @param folder main model folder
#' @param filename by default will look for NONMEM nmfe results file, but can also
#' look e.g. for a table. In that case specify `filename`.
#' @param fallback fallback filename
#'
find_file_with_fallback <- function(folder, filename, fallback, verbose = TRUE) {
  f <- file.path(folder, filename)
  if(! file.exists(f)) {
    f <- file.path(folder, fallback)
    if(! file.exists(f)) {
      if(verbose)
        cli::cli_abort("An expected file was not found.")
    } else {
      if(verbose)
        cli::cli_alert_warning("File found in fallback location.")
    }
  }
  f
}
