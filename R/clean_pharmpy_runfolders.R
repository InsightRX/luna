#' Clean pharmpy run folders like modelfit1 etc
#'
#' @inheritParams run_nlme
#' @param clean should folders really be removed (`TRUE`), or just show a warning (`FALSE`)
#'
#' @export
#'
clean_pharmpy_runfolders <- function(
  id = NULL,
  folder,
  tool,
  remove = TRUE,
  verbose = TRUE
) {
  tool_runfolders <- get_pharmpy_runfolders(id = id, folder = folder, tool = tool)
  if(length(tool_runfolders) > 0) {
    if(remove) {
      cli::cli_alert_info("Cleaning {length(tool_runfolders)} existing {tool} folders")
      for(f in tool_runfolders) {
        if(!is.null(id)) {
          full_folder_path <- file.path(folder, id, f)
        } else {
          full_folder_path <- file.path(folder, f)
        }
        if(f != "") {
          unlink(full_folder_path, recursive = TRUE, force = TRUE)
        }
      }
    } else {
      cli::cli_alert_info("Leaving {length(tool_runfolders)} existing {tool} folders. Use `clean=TRUE` to remove.")
    }
  }
}
