#' Create PK mmodel search space definition for pharmpy `modelsearch`
#'
#' See Pharmpy MFL documentation for more info:
#' https://pharmpy.github.io/latest/modelsearch.html
#'
#' @param absorption absorption model options
#' @param elimination elimination model options
#' @param peripherals peripheral compartment options
#' @param transits transit model options
#' @param lagtime lagtime options
#'
#' @export
#'
create_pkmodel_search_space <- function(
  absorption = c("FO", "ZO"),
  elimination = c("FO", "MM"),
  peripherals = c(0, 1),
  transits = c(0, 1, 3),
  lagtime = c("OFF", "ON")
) {

  ## Confirm all requested options are allowed
  all_options <- list(
    ABSORPTION = c("INST", "FO", "ZO", "SEQ-ZO-FO"),
    ELIMINATION = c("FO", "ZO", "MM", "MIX-FO-MM"),
    PERIPHERALS = c("number", "DRUG", "MET"),
    TRANSITS = c("number", "DEPOT", "NODEPOT"),
    LAGTIME = c("OFF", "ON")
  )
  args <- c("absorption", "transits", "lagtime", "elimination", "peripherals")
  out <- c()
  for(key in args) {
    value <- get(key)
    if(!is.null(value) & length(value) > 0) {
      tmp <- value
      tmp[is.numeric(tmp)] <- rep("number", sum(is.numeric(tmp)))
      if (! all(tmp %in% all_options[[toupper(key)]])) {
        cli::cli_abort("Some options not recongized: {}")
      }
      out <- c(
        out,
        paste0(toupper(key), "([", paste0(value, collapse=","), "])")
      )
    }
  }

  paste(out, collapse = "; ")

}
