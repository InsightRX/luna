#' Creates a ggplot2 plot with predictions and observations, split
#' by subject
#'
#' @inheritParams luna_run
#' @param data instead of getting an output table from a run, supply a
#' data.frame with the relevant data columns.
#' @param smooth_method ggplot2-supported smooth method, e.g. "loess"
#' @param ltbs log-transform-both-sides error model? If `TRUE`, will
#' exponentiate DV, PRED, and IPRED.
#' @param exponentiate exponentiate DV, PRED, and IPRED columns?
#' @param se_factor when plotting uncertainty for IPRED, factor to multiply
#' the SE with. Default is 1, i.e. show the standard error. If set to 1.96,
#' it will show the 95% CI. (Use `qnorm()` to find relevant factor for CI)
#' @param ncol number of faceting columns to show
#' @param nrow number of faceting rows to show
#' @param page page number of plots to show
#' @param scales passed to `facet_wrap()` function as the `scales`
#' argument, can be "free", "free_x", or "free_y".
#' @param subject_ids list of subject IDs to be plotted.
#'
#' @export
#'
luna_ind <- function(
    id,
    folder = NULL,
    data = NULL,
    theme = ggplot2::theme_classic,
    smooth_method = "loess",
    exponentiate = FALSE,
    show = list(
      dv = TRUE,
      ipred = TRUE,
      pred = TRUE,
      doses = TRUE,
      se = TRUE
    ),
    se_factor = 1,
    ltbs = FALSE,
    ncol = 3,
    nrow = 3,
    page = 1,
    scales = NULL,
    verbose = TRUE
) {

  ## determine what to show
  show <- replace_list_elements(
    list(
      dv = TRUE,
      ipred = TRUE,
      pred = TRUE,
      doses = TRUE,
      se = TRUE
    ),
    show
  )

  if(is.null(data)) {
    tab <- get_table_for_plots(
      id = id,
      folder = folder,
      residual = NULL,
      ltbs = ltbs,
      verbose = verbose
    )
  } else {
    tab <- data
  }

  ## Show SE for IPRED?
  if(show$se) {
    has_se <- "IPRED_SE" %in% names(tab)
    if(has_se) {
      tab <- tab |>
        dplyr::mutate(
          IPRED_min = IPRED - IPRED_SE * abs(se_factor),
          IPRED_max = IPRED + IPRED_SE * abs(se_factor)
        )
    }
  } else {
    has_se <- FALSE
  }

  ## Exponentiate?
  if(exponentiate) {
    exp_safe <- function(x) {
      dplyr::if_else(is.na(x), 0, exp(x))
    }
    tab <- tab |>
      dplyr::mutate(
        dplyr::across(
          c(DV, PRED, IPRED),
          exp_safe
        )
      )
    if(show$se && has_se) {
      tab <- tab |>
        dplyr::mutate(
          dplyr::across(
            c(IPRED_min, IPRED_max),
            exp_safe
          )
        )
    }
  }

  ## Pagination
  ids <- unique(tab$ID)
  if (!is.null(subject_ids)){
    ids <- subject_ids
  }
  set_len <- ncol * nrow

  start_idx <- set_len * (page-1) + 1
  end_idx <- set_len * page
  max_pages <- floor(length(ids) / (ncol * nrow))
  if(page > max_pages) {
    cli::cli_abort("Sorry, only {max_pages} pages available.")
  }
  if(end_idx > length(ids)) {
    cli::cli_alert_info("Only first {set_len} IDs will be shown.")
    end_idx <- length(ids)
  }
  if(verbose) {
    cli::cli_alert_info("Showing page {page}/{max_pages}.")
  }
  ids_sel <- ids[start_idx:end_idx]
  cols <- c()
  if(show$pred) cols <- c(cols, "PRED")
  if(show$ipred) cols <- c(cols, "IPRED")

  ## Doses?
  tab_doses <- tab |>
    dplyr::filter(ID %in% ids_sel) |>
    dplyr::filter(EVID == 1)

  ## PRED/IPRED vs DV
  tab_long <- tab |>
    dplyr::filter(ID %in% ids_sel) |>
    dplyr::filter(MDV == 0) |>
    tidyr::pivot_longer(cols = !!cols)
  p1 <- tab_long |>
    ggplot2::ggplot(
      ggplot2::aes(x = TIME, y = value, colour = name)
    )
  if(show$doses) {
    p1 <- p1 +
      ggplot2::geom_vline(
        data = tab_doses,
        ggplot2::aes(xintercept = TIME),
        colour = "#cccccc"
      )
  }
  if(isTRUE(show$dv)) {
    p1 <- p1 +
      ggplot2::geom_point(
        data = tab |>
          dplyr::filter(ID %in% ids_sel) |>
          dplyr::filter(EVID == 0),
        ggplot2::aes(y = DV),
        colour = "black"
      )
  }
  p1 <- p1 +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_wrap(~ID, scales = scales) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Concentration") +
    theme() +
    ggplot2::theme(
      legend.title = element_blank(),
      legend.position = "top"
    )

  ## Add uncertainty?
  if(isTRUE(has_se)) {
    p1 <- p1 +
      ggplot2::geom_ribbon(
        data = tab_long |> dplyr::filter(name == "IPRED"),
        ggplot2::aes(x = TIME, ymin = IPRED_min, ymax = IPRED_max),
        colour = NA,
        alpha = 0.13
      )
  }

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
