#' Show a log of events (actions and errors)
#'
#' @param n number of most recent events to return, default is 20
#'
#' @export
#'
luna_log <- function(n = 20) {
  is_luna_cache_available(abort = TRUE)
  .luna_cache$get("log") |>
    utils::tail(n = n) |>
    lapply(FUN = function(x) {
      data.frame(
        event = x$event,
        action = x$action,
        id = x$id,
        time = paste0(get_time_ago(x$timestamp), " ago")
      )
    }) |>
    dplyr::bind_rows()
}

#' Add event to log
#'
#' @param event type of event (`action` or `error`)
#' @param action action type, e.g. `modelfit`
#' @param id run id
#' @param context other information, e.g. error message
#'
log_add <- function(
  event = c("action", "error"),
  action = "modelfit",
  id,
  context = NA
) {
  event <- match.arg(event)
  is_luna_cache_available(abort = TRUE)
  luna_log <- .luna_cache$get("log")
  if(is.null(luna_log) || length(luna_log) == 0)
    luna_log <- list()
  timestamp <- lubridate::now() |>
    lubridate::format_ISO8601()
  luna_log <- c(
    luna_log,
    list(
      list(
        event = event,
        action = action,
        id = id,
        context = context,
        timestamp = timestamp
      )
    )
  )
  .luna_cache$set("log", luna_log)
}

#' Get last event from log, as list object
#'
log_get_last_event <- function() {
  .luna_cache$get("log")[[length(.luna_cache$get("log"))]]
}
