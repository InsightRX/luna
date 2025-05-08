#' Parse NONMEM model file into a list containing blocks of code
#'
#' @param modelfile NONMEM model filename
#' @param as_block import code blocks as block of text (`TRUE`, default) or as
#' separate lines (`FALSE`)
#' @param code NONMEM code (alternative to specifying file name)
#'
#' @export
nm_read_model <- function(
    modelfile = NULL,
    as_block = FALSE,
    code = NULL) {
  if(is.null(modelfile)) {
    if(is.null(code)) {
      cli::cli_abort("Please specify a NONMEM modelfile or NONMEM code.")
    }
  }
  if(!is.null(code)) {
    nm_txt <- code
  } else {
    if(file.exists(modelfile)) {
      nm_txt <- readChar(modelfile, file.info(modelfile)$size)
    } else {
      cli::cli_abort(paste0("NONMEM modelfile (", modelfile,") not found."))
    }
  }
  nm_lines <- stringr::str_split(nm_txt, "\\n")[[1]]
  # remove any spaces before start of line
  for(i in seq(nm_lines)) {
    nm_lines[i] <- stringr::str_replace_all(nm_lines[i], "^[\\s\\t]*", "")[[1]]
  }
  # get block indices:
  block_idx <- c(1:length(nm_lines))[stringr::str_detect(nm_lines, "^\\$")]
  if(length(block_idx) == 0) {
    print(nm_lines)
    cli::cli_abort("Sorry, no code blocks detected in NONMEM file.")
  }
  block_idx <- c(block_idx, length(nm_lines)+1)
  obj <- list()
  for(i in 1:(length(block_idx)-1)) {
    block_id <- stringr::str_replace(
      stringr::str_split(nm_lines[block_idx[i]], "\\s")[[1]][1],
      "^\\$", "")
    block_tmp <- nm_lines[block_idx[i]:(block_idx[i+1]-1)]
    if(as_block) {
      block_tmp <- stringr::str_c(block_tmp, collapse="\n")
    }
    if(is.null(obj[[block_id]])) {
      obj[[block_id]] <- block_tmp
    } else {
      obj[[block_id]] <- c(obj[[block_id]], block_tmp)
    }
  }
  class(obj) <- c("NONMEM", "list")
  return(obj)
}
