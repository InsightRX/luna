#' Get table for basic GOF plots based on run id
#'
#' @inheritParams luna_gof
#'
#' @export
#'
get_table_for_plots <- function(
  id,
  folder = NULL,
  residual = c("CWRES", "NPDE"),
  ltbs = FALSE,
  verbose = TRUE
) {

  id <- validate_id(id)
  if(is.null(folder)) {
    folder <- .luna_cache$get("project")$metadata$folder
  }
  residual <- match.arg(residual)

  ## Get tables from run
  tables <- luna_tables(
    id,
    folder = folder,
    verbose = verbose
  )

  ## Check that required data is available
  reqd <- c("DV", "PRED", "IPRED", residual, "TIME", "EVID")
  tab_sel <- NULL
  for(tab in tables) {
    if(all(reqd %in% names(tab))) {
      tab_sel <- tab
      break()
    }
  }
  if (is.null(tab_sel)) {
    cli::cli_abort(
      paste0(
        "Could not find a table with all required variables: ",
        paste0(reqd, collapse = ", ")
      )
    )
  }

  if (ltbs) {
    tab_sel <- tab_sel |>
      dplyr::mutate(
        PRED = dplyr::if_else(EVID == 0, exp(PRED), PRED),
        IPRED = dplyr::if_else(EVID == 0, exp(IPRED), IPRED),
        DV = dplyr::if_else(EVID == 0, exp(DV), DV)
      )
  }

  tab_sel

}
