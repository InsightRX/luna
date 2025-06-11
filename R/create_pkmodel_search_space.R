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
  for(key in args) {
    tmp <- get(key)
    tmp[is.numeric(tmp)] <- rep("number", sum(is.numeric(tmp)))
    if (! all(tmp %in% all_options[[toupper(key)]])) {
      cli::cli_abort("Some options not recongized: {}")
    }
  }

  ## combine into a MFL search space definition
  paste0(c(
    paste0("ABSORPTION:[", paste0(absorption, collapse=","), "]"),
    paste0("ELIMINATION:[", paste0(elimination, collapse=","), "]"),
    paste0("PERIPHERALS:[", paste0(peripherals, collapse=","), "]"),
    paste0("TRANSITS:[", paste0(transits, collapse=","), "]"),
    paste0("LAGTIME:[", paste0(lagtime, collapse=","), "]")
  ), collapse = "; ")
}
