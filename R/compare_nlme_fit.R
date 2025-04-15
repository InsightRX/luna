#' Compare fit of two or more NLME fits
#'
#' @param ... fit objects
#' @param return_object logical, if TRUE, return a list of the combined info and parameter tables
#' @export
#'
compare_nlme_fit <- function(..., return_object = FALSE) {
  fits <- list(...)
  if(length(fits) == 1) {
    if(length(fits[[1]]) > 1) {
      fits <- fits[[1]]
    }
  }
  ## First, combine into a list of parsed info
  fit_info <- purrr::map(fits, function(x) {
    list(
      info_tab = create_modelfit_info_table(x),
      par_tab = create_modelfit_parameter_table(x),
      name = attr(x, "model")$name
    )
  })
  ## Then grab the right info and combine columns from different runs
  info_comb <- combine_info_columns(
    fit_info,
    "info_tab",
    label = "Detail"
  )
  par_comb <- combine_info_columns(
    fit_info,
    "par_tab",
    label = "Parameter"
  )
  if(return_object) {
    return(list(
      info_comb = info_comb,
      par_comb = par_comb
    ))
  } else {
    print(
      knitr::kable(info_comb)
    )
    print(
      knitr::kable(par_comb)
    )
  }
}

#' Combine columns with run info into a data.frame
#' and make sure that rows match (e.g. parameters)
#'
#' data.frames in list should have the same column names but can have different
#' row names (e.g. parameter names).
#'
combine_info_columns <- function(
    fit_info,
    table = "info_tab",
    label = "Detail"
) {
  comb <- dplyr::bind_rows(
    purrr::map(fit_info, function(x) {
      cols <- data.frame(x[[table]][,-1])
      rownames(cols) <- x[[table]][,1]
      t(cols) %>% data.frame() # leverage bind_rows to match parameter names and insert NAs. bind_cols cannot do that.
    })
  ) %>%
    t() %>% data.frame() # pivot back again
  run_names <- unlist(purrr::map(fit_info, function(x) x$name ))
  table_colnames <- colnames(fit_info[[1]][[table]][,-1])
  cols <- paste0(
    rep(run_names, each = length(table_colnames)),
    ": ",
    rep(table_colnames, length(run_names))
  )
  names(comb) <- cols
  comb
}
