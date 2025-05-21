#' Creates a ggplot2 panel object with basic goodness of fit plots
#'
#' @param id run id
#'
#' @export
#'
luna_gof <- function(
  id,
  theme = ggplot2::theme_classic,
  folder = NULL
) {
  if(is.null(folder)) {
    is_luna_cache_available(abort = TRUE)
    folder <- .luna_cache$get("project")$metadata$folder
  }
  model_file <- paste0(id, ".mod")
  model <- pharmr::read_model(model_file)
  tables <- get_tables_from_fit(
    model,
    path = folder
  )
  reqd <- c("DV", "PRED", "IPRED", "CWRES", "TIME", "EVID")
  tab_sel <- NULL
  for(tab in tables) {
    if(all(reqd %in% names(tab))) {
      tab_sel <- tab
      break()
    }
  }
  if(is.null(tab_sel)) {
    cli::cli_abort(
      paste0(
        "Could not find a table with all required variables: ",
        paste0(reqd, collapse = ", ")
      )
    )
  }
  tab_sel |>
    dplyr::filter(MDV == 0) |>
    ggplot2::ggplot(
      ggplot2::aes(x = PRED, y = DV)
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
      theme()
}
