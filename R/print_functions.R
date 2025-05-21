#' Prints a list of models / runs in project
#'
#' @param x luna project object
#'
#' @export
print.luna.run_table <- function(
  x,
  format = "simple",
  verbose = FALSE,
  ...
) {

  if(nrow(x) > 0) {
    # Use knitr to create a nice table
    if(verbose)
      cli::cli_alert_info("Loading models in project:")
    knitr::kable(
      x |>
        dplyr::arrange(
          stringr::str_rank(id, numeric = TRUE)
        ),
      format = format
    ) |>
      writeLines()
  } else {
    if(verbose)
      cli::cli_alert_info("No models found in project.")
  }

}

#' Apply column width truncation based on console width and relative width specs
#'
truncate_columns <- function(
  df,
  width_specs,
  total_width = NULL
) {
  ## TODO: this could be made smarter, if it also looked whether some columns
  ##       actually have data to fill the allowed with or not. But perhaps
  ##       there are existing tools for this.
  if(is.null(total_width))
    total_width <- getOption("width")
  result <- df
  all_widths <- sapply(df, function(col) max(nchar(col), na.rm = TRUE))
  fixed_widths <- all_widths[is.na(width_specs)]
  remaining_width <- total_width - sum(fixed_widths)
  scale <- 1/sum(as.numeric(width_specs[!is.na(width_specs)]))
  for(col in names(width_specs)) {
    if(!is.na(width_specs[[col]])) {
      abs_width <- floor(scale * remaining_width * width_specs[[col]])
      if(abs_width < 1) {
        cli::cli_abort("Information does not fit on the console, please increase console width.")
      }
      result[[col]] <- stringr::str_trunc(df[[col]], abs_width)
    }
  }
  result
}
