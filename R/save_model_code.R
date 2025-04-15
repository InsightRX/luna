#' Save model code to a markdown file
#' 
#' @param model nlmixr2 model object
#' @param path path to .md file to save model code to
#' 
#' @export
save_model_code <- function(
  model,
  path
) {
  
  ## Works for nlmixr2 models. Will need to adapt for NONMEM models
  md <- paste(
    "## Model code",
    "",
    "```",
    model,
    "```",
    sep = "\n"
  )

  writeLines(md, path)
}
