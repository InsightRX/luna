#' Create a single regimen
#'
#' The resulting data.frame can be passed to `luna::run_sim()` as the `regimen`
#' argument.
#'
#' @examples
#'
#' \dontrun{
#' reg1 <- create_regimen(
#'   dose = 500,
#'   interval = 12,
#'   n = 10,
#'   route = "oral"
#' )
#' luna::run_sim(..., regimen = reg1)
#' }
#'
#' @export
create_regimen <- function(
    dose,
    interval = 24,
    n,
    t_inf = NULL,
    route = c("oral", "iv", "sc", "im")
) {
  route <- match.arg(route)
  if(route %in% c("iv", "im", "sc")) {
    if(is.null(t_inf)) {
      cli::cli_alert_info("`t_inf` not specified, assuming bolus dose.")
      t_inf <- 0
    }
  } else {
    t_inf <- 0
  }
  out <- data.frame(
    dose = rep(dose, n),
    time = seq(0, to = interval * (n - 1), by = interval),
    route = rep(route, n),
    t_inf = rep(t_inf, n),
    interval = rep(interval, n)
  )
  class(out) <- c("dosing_regimen", "data.frame")
  out
}

#' Combine several regimens into a single data.frame, which can be passed into
#' `luna::run_sim()` as `regimen` argument.
#'
#' @details
#' This allows both for combination of two or more phases, e.g. loading doses
#' and maintenance phase in a single regimen. It also allows for specification
#' of multiple separate regimens to simulate, e.g. a high-dose regimen and a
#' low-dose regimen.
#'
#' @param ... each argument is a named regimen, that in itself is specified as
#' a list containing multiple regimens, each created using `create_regimen()`.
#' See examples.
#'
#' @examples
#' \dontrun{
#' regimens <- combine_regimens(
#'   "without_load" = list(
#'     create_regimen(
#'       dose = 500,
#'       interval = 12,
#'       n = 10,
#'       route = "oral"
#'     )
#'   ),
#'   "with_load" = list(
#'     create_regimen(
#'       dose = 2000,
#'       n = 1,
#'       interval = 12,
#'       route = "iv",
#'       t_inf = 1
#'     ),
#'     create_regimen(
#'       dose = 500,
#'       n = 5,
#'       interval = 24,
#'       route = "oral"
#'     )
#'  )
#' )
#' }
#'
#' @export
combine_regimens <- function(...) {
  regs <- list(...)
  regimen_names <- names(regs)
  if(is.null(regimen_names)) {
    cli::cli_abort("Please use named arguments to `combine_regimen()` to define the regimen names.")
  }
  comb <- data.frame()
  for(key in names(regs)) {
    t_last <- 0
    for(idx in seq(regs[[key]])) {
      tmp <- regs[[key]][[idx]]
      if(!inherits(tmp, "dosing_regimen")) {
        cli::cli_abort("Please create dosing regimens using the `create_regimen()` function.")
      }
      tmp <- tmp |>
        dplyr::mutate(time = time + t_last) |>
        dplyr::mutate(regimen = key)
      comb <- dplyr::bind_rows(comb, tmp)
      t_last <- max(tmp$time) + tail(tmp$interval, 1)
    }
  }
  comb
}
