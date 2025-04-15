#' Update $DATA in NONMEM model with new dataset
#' 
#' @param model_filename model filename
#' @param dataset_filename dataset filename
#' @param ... parameters passed to `nm_save_model()`
#' 
#' @export
#' 
nm_update_dataset <- function(model_filename, dataset_filename, ...) {
  mod <- nm_read_model(model_filename)
  data <- mod$DATA
  # Assume that the first block of text that does not have $DATA, IGNORE=, 
  # or ACCEPT= is the dataset.
  for(i in seq(data)) {
    line <- data[i]
    line_vec <- unlist(stringr::str_split(line, " "))
    idx_not_dataset <- grep("(\\$DAT)|(IGNORE=)|(ACCEPT=)", line_vec)
    idx_dataset <- seq(line_vec)[-idx_not_dataset][1]
    if(length(idx_dataset) > 0) {
      line_vec[idx_dataset] <- dataset_filename
      data[i] <- paste(line_vec, collapse = " ")
      mod$DATA <- data
      nm_save_model(
        mod, 
        modelfile = model_filename, 
        ...
      )
      break
    }
  }
}