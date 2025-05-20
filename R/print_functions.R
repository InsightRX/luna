#' Prints a list of models / runs in project
#'
#' @param project luna project object
#'
#' @export
print.luna.project <- function(
  x,
  format = "simple",
  ...
) {

  models <- x$yaml$runs$modelfit
  folder <- x$metadata$folder
  name <- x$metadata$name

  ## check if cache is present
  is_cache_available <- is_luna_cache_available(abort = TRUE)

  ## Print a table of models and their descriptions
  if(length(models) > 0) {
    timestamps <- .luna_cache$get("timestamps")
    model_table <- data.frame(
      # "type" = "modelfit",
      "id" = sapply(models, function(y) {
        y$id
      }),
      "reference" = sapply(models, function(y) {
        ifelse0(y$reference, "")
      }),
      "description" = sapply(models, function(y) {
        ifelse0(y$description, "")
      }),
      "notes" = sapply(models, function(y) {
        ifelse0(y$notes, "")
      }),
      "status" = sapply(models, function(y) {
        ifelse0(get_status(y$id, folder), "")
      }),
      "finished" = sapply(models, function(y) {
        ifelse0(get_time_ago(timestamps$results[[y$id]]), "")
      })
    )
    column_widths <- list(
      id = NA,
      reference = 1,
      description = 2,
      notes = 2,
      status = NA,
      finished = NA
    )
    model_table <- truncate_columns(
      df = model_table,
      width_specs = column_widths
    )

    # Use knitr to create a nice table
    cli::cli_alert_info("Loading models in project {name}:")
    knitr::kable(
      model_table |>
        dplyr::arrange(
          stringr::str_rank(id, numeric = TRUE)
        ),
      format = format
    ) |>
      writeLines()
  } else {
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
      result[[col]] <- stringr::str_trunc(df[[col]], abs_width)
    }
  }
  result
}
