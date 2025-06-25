#' For a given run, get info on what folders are available for selected
#' Pharmpy or PsN tools
#'
#' @inheritParams luna_info
#'
#' @param tools vector of tools for which to find info
#'
get_tools_info <- function(id, folder, tools = c()) {
  run_folder <- file.path(folder, id)
  folders <- list.dirs(run_folder, full.names = FALSE, recursive = FALSE)
  tool_list <- list()
  for(tool in tools) {
    tool_list[tool] <- folders[stringr::str_detect(folders, tool)]
  }
  tool_list
}
