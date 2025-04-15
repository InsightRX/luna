#' Save an R data.frame to a NONMEM-style dataset as CSV
#' 
#' @param data data.frame
#' @param filename filename to save to.
#' @param tool type of dataset, either `nonmem` or `nlmixr`
nm_save_dataset <- function(
  data, 
  filename, 
  tool = c("nonmem", "nlmixr")
) {
  tool <- match.arg(tool)
  if(tool == "nonmem") {  ## Remove NAs for NONMEM, and replace with ".". Not for nlmixr
    for(key in names(dat)) {
      data[[key]] <- as.character(data[[key]])
      data[[key]][is.na(data[[key]])] <- "."
    }
  }
  write.csv(data, filename, quote=F, row.names=F)
}