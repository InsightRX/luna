#' Convert runs in a project object to a data.frame
#'
runs_as_table <- function(x) {

  ## check if cache is present
  is_cache_available <- is_luna_cache_available(abort = TRUE)

  ## Print a table of models and their descriptions
  models <- x$yaml$runs$modelfit
  model_table <- data.frame()
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
        ifelse0(get_status(y$id, x$metadata$folder), "")
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
  }

  ## Return as tibble with custom luna class
  model_table <- dplyr::as_tibble(model_table)
  class(model_table) <- c("luna.run_table", class(model_table))
  model_table
}
