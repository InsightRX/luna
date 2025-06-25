#' Find last pharmpy run folder
#'
#' @inheritParams call_pharmpy_tool
#'
get_pharmpy_runfolders <- function(
  id = NULL,
  folder = NULL,
  tool
) {
  if(is.null(folder)) {
    folder <- getwd()
  }
  if(!is.null(id)) {
    fit_folder <- file.path(folder, id)
  } else {
    fit_folder <- folder
  }
  tool_dirs <- list.dirs(
    path = fit_folder,
    recursive = FALSE,
    full.names = FALSE
  )
  pattern <- paste0("^", tool, "[0-9]+?$")
  tool_dirs <- tool_dirs[stringr::str_detect(tool_dirs, pattern)]
  last_dir <- tool_dirs[order(stringr::str_rank(tool_dirs, numeric = TRUE))]
  last_dir
}
