#' Creates a ggplot2 panel object with basic goodness of fit plots
#'
#' @inheritParams luna_run
#' @param residual either "CWRES" or "NPDE"
#' @param smooth_method ggplot2-supported smooth method, e.g. "loess"
#' @param ltbs log-transform-both-sides error model? If `TRUE`, will
#' exponentiate DV, PRED, and IPRED.
#'
#' @export
#'
luna_gof <- function(
  id,
  folder = NULL,
  residual = c("CWRES", "NPDE"),
  theme = ggplot2::theme_classic,
  smooth_method = "loess",
  ltbs = FALSE,
  verbose = TRUE
) {

  tab <- get_table_for_plots(
    id = id,
    folder = folder,
    residual = residual,
    ltbs = ltbs,
    verbose = verbose
  )

  ## PRED/IPRED vs DV
  p1 <- tab |>
    dplyr::filter(MDV == 0) |>
    tidyr::pivot_longer(cols = c(IPRED, PRED)) |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = DV)
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = smooth_method, formula = y ~ x) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
      ggplot2::facet_wrap(~name) +
      ggplot2::xlab("Prediction") +
      ggplot2::ylab("Obervation (DV)") +
      theme()
  ## CWRES vs TIME / PRED
  p2 <- tab |>
    dplyr::filter(MDV == 0) |>
    tidyr::pivot_longer(cols = c(TIME, PRED)) |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = CWRES)
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = smooth_method, formula = y ~ x) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::facet_wrap(~name, scales = "free") +
    ggplot2::xlab("") +
    ggplot2::ylab("Obervation (DV)") +
    theme()

  ## Add log event
  log_add(
    event = "plot",
    action = "modelfit",
    id = id,
    context = list(
      plot = "gof"
    )
  )

  patchwork::wrap_plots(
    p1, p2,
    ncol = 1
  )
}
