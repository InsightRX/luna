#' Convert runs in a project object to a data.frame
#'
runs_as_table <- function(x) {

  ## check if cache is present
  is_cache_available <- is_luna_cache_available(abort = TRUE)

  ## Print a table of models and their descriptions
  models <- x$yaml$runs
  model_table <- data.frame()
  if(length(models) > 0) {
    timestamps <- .luna_cache$get("timestamps")
    model_table <- data.frame(
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
      "output_file" = sapply(models, function(y) {
        ifelse0(find_file_with_fallback(
          folder = x$metadata$folder,
          filename = file.path(y$id, paste0("run", ".lst")),
          fallback = paste0(y$id, ".lst"),
          verbose = FALSE,
          abort = FALSE
        ), "")
      }),
      "tools" = sapply(models, function(y) {
        ifelse0(get_tools(y), "")
      })
    )
    model_table$status <- sapply(model_table$id, function(y) {
      ifelse0(get_status(y, x$metadata$folder), "")
    })
    model_table$finished <- sapply(models, function(y) {
      ifelse0(get_time_ago(timestamps$results[[y$id]]), "")
    })
    column_widths <- list(
      id = NA,
      reference = 1,
      description = 2,
      notes = 2,
      status = NA,
      finished = NA,
      tools = 1.5
    )
    model_table <- truncate_columns(
      df = model_table |>
        dplyr::select(-output_file),
      width_specs = column_widths
    )
  }

  ## Return as tibble with custom luna class
  model_table <- dplyr::as_tibble(model_table)
  class(model_table) <- c("luna.run_table", class(model_table))
  model_table
}

get_tools <- function(run) {
  paste0(
    unique(
      unlist(lapply(run$tools, function(x) x$tool))
    ),
    collapse = ","
  )
}
