#' Creates a ggplot2 panel object with basic goodness of fit plots
#'
#' @param id run id
#' @param residual either "CWRES" or "NPDE"
#'
#' @export
#'
luna_gof <- function(
  id,
  residual = c("CWRES", "NPDE"),
  theme = ggplot2::theme_classic,
  folder = NULL
) {
  residual <- match.arg(residual)
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
  reqd <- c("DV", "PRED", "IPRED", residual, "TIME", "EVID")
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
  ## PRED/IPRED vs DV
  p1 <- tab_sel |>
    dplyr::filter(MDV == 0) |>
    tidyr::pivot_longer(cols = c(IPRED, PRED)) |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = DV)
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth() +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
      ggplot2::facet_wrap(~name) +
      xlab("Prediction") +
      ylab("Obervation (DV)") +
      theme()
  ## CWRES vs TIME / PRED
  p2 <- tab_sel |>
    dplyr::filter(MDV == 0) |>
    tidyr::pivot_longer(cols = c(TIME, PRED)) |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = CWRES)
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::facet_wrap(~name, scales = "free") +
    xlab("") +
    ylab("Obervation (DV)") +
    theme()
  patchwork::wrap_plots(
    p1, p2,
    ncol = 1
  )
}
