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

  ## Get cache info:
  cache_file <- file.path(folder, paste0(".luna.", name, ".cache"))
  if(file.exists(cache_file)) {
    cache <- yaml::read_yaml(cache_file)
  } else {
    cli::cli_alert_warning("Luna cache for project not found.")
    cache <- list()
  }

  ## Print a table of models and their descriptions
  if(length(models) > 0) {
    model_table <- data.frame(
      # "type" = "modelfit",
      "id" = sapply(models, function(y) {
        stringr::str_trunc(y$id, 12)
      }),
      "reference" = sapply(models, function(y) {
        ifelse0(stringr::str_trunc(y$reference, 12), "")
      }),
      "description" = sapply(models, function(y) {
        ifelse0(stringr::str_trunc(y$description, 35), "")
      }),
      "notes" = sapply(models, function(y) {
        ifelse0(stringr::str_trunc(y$notes, 25), "")
      }),
      "status" = sapply(models, function(y) get_status(y$id, folder)),
      "finished" = sapply(models, function(y) {
        get_time_ago(cache$timestamps$results[[y$id]])
      })
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
