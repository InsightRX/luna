#' Find last pharmpy run folder
#'
#' @inheritParams call_pharmpy_tool
#'
get_last_pharmpy_runfolder <- function(id, folder = NULL, tool) {
  if(is.null(folder)) {
    folder <- getwd()
  }
  fit_folder <- file.path(folder, id)
  tool_dirs <- list.dirs(
    path = fit_folder,
    recursive = FALSE,
    full.names = FALSE
  )
  pattern <- paste0("^", tool, "[0-9]+?$")
  tool_dirs <- tool_dirs[stringr::str_detect(tool_dirs, pattern)]
  last_dir <- tool_dirs[which.max(stringr::str_rank(tool_dirs, numeric = TRUE))]
  last_dir
}
