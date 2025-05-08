#' Prints a list of models / runs in project
#'
#' @param project luna project object
#'
#' @export
print.luna.project <- function(
  x,
  arrange = c("type", "status", "id"),
  format = "simple",
  ...
) {

  models <- x$yaml$runs$modelfit
  folder <- x$metadata$folder
  name <- x$metadata$name

  ## Print a table of models and their descriptions
  if(length(models) > 0) {
    model_table <- data.frame(
      "type" = "modelfit",
      "id" = sapply(models, function(y) {
        stringr::str_trunc(y$id, 12)
      }),
      "description" = sapply(models, function(y) {
        ifelse0(stringr::str_trunc(y$description, 35), "")
      }),
      "notes" = sapply(models, function(y) {
        ifelse0(stringr::str_trunc(y$notes, 25), "")
      }),
      "status" = sapply(models, function(y) get_status(y$id, folder))
    )
    # Use knitr to create a nice table
    cli::cli_alert_info("Loading models in project {name}:")
    knitr::kable(
      model_table |>
        dplyr::arrange_at(arrange),
      format = format
    ) |>
      writeLines()
  } else {
    cli::cli_alert_info("No models found in project.")
  }

}
