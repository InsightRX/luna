#' Creates a ggplot2 plot with predictions and observations, split
#' by subject
#'
#' @inheritParams luna_run
#' @param smooth_method ggplot2-supported smooth method, e.g. "loess"
#' @param ltbs log-transform-both-sides error model? If `TRUE`, will
#' exponentiate DV, PRED, and IPRED.
#' @param ncol number of faceting columns to show
#' @param nrow number of faceting rows to show
#' @param page page number of plots to show
#' @param scales passed to `facet_wrap()` function as the `scales`
#' argument, can be "free", "free_x", or "free_y".
#'
#' @export
#'
luna_ind <- function(
  id,
  folder = NULL,
  theme = ggplot2::theme_classic,
  smooth_method = "loess",
  ltbs = FALSE,
  ncol = 3,
  nrow = 3,
  page = 1,
  scales = NULL,
  verbose = TRUE
) {

  tab <- get_table_for_plots(
    id = id,
    folder = folder,
    residual = NULL,
    ltbs = ltbs,
    verbose = verbose
  )

  ## Pagination
  ids <- unique(tab$ID)
  set_len <- ncol * nrow
  start_idx <- set_len * (page-1) + 1
  end_idx <- set_len * page
  max_pages <- floor(length(ids) / (ncol * nrow))
  if(page > max_pages) {
    cli::cli_abort("Sorry, only {max_pages} pages available.")
  }
  if(end_idx > length(ids)) {
    end_idx <- length(ids)
  }
  if(verbose) {
    cli::cli_alert_info("Showing page {page}/{max_pages}.")
  }
  ids_sel <- ids[start_idx:end_idx]

  ## PRED/IPRED vs DV
  tab_long <- tab |>
    dplyr::filter(ID %in% ids_sel) |>
    dplyr::filter(MDV == 0) |>
    tidyr::pivot_longer(cols = c(IPRED, PRED))
  p1 <- tab_long |>
    ggplot2::ggplot(
      ggplot2::aes(x = TIME, y = value, colour = name)
    ) +
    ggplot2::geom_point(
      data = tab_long |> dplyr::filter(name == "PRED"),
      ggplot2::aes(y = DV),
      colour = "black"
    ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ID, scales = scales) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Concentration") +
    theme()

  ## Add log event
  log_add(
    event = "plot",
    action = "modelfit",
    id = id,
    context = list(
      plot = "ind"
    )
  )

  p1
}
