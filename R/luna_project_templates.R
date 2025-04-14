#' Show available templates
#'
#' @export
#'
luna_project_templates <- function() {
  templates <- stringr::str_replace_all(
    dir(system.file("templates", package = "luna"), pattern = ".yaml$"),
    "\\.yaml$",
    ""
  )
  cat("Available templates:\n")
  cat(paste0(" - ", templates, collapse = "\\n"))
}
