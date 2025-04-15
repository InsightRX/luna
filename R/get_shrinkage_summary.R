#' Parses a NONMEM output file and extracts shrinkage
#' 
#' @param fit pharmpy model object
#' @param path path to nonmem output file (.lst)
#' 
get_shrinkage_summary <- function(path = NULL, fit = NULL) {
  if(is.null(path) || !file.exists(path)) {
    return(list())
  }
  txt <- readLines(path)
  eta <- get_shrinkage_values(txt, "ETASHRINKSD")
  ebv <- get_shrinkage_values(txt, "EBVSHRINKSD") # based on first order approximation of the posterior variance around the mode (see intro to NM7 pdf, p231)
  eps <- get_shrinkage_values(txt, "EPSSHRINKSD")
  if(!is.null(fit$individual_estimates)) {
    eta_names <- names(fit$individual_estimates)
  } else {
    eta_names <- paste0("ETA_", seq(length(eta)))
  }
  names(eta) <- eta_names
  names(ebv) <- eta_names
  list(
    eta = eta,
    ebv = ebv,
    eps = eps
  )
}

#' Get shrinkage values from a single line in NONMEM output
#'
get_shrinkage_values <- function(
    txt, 
    type = "ETASHRINKSD"
) {
  spl <- txt[stringr::str_detect(txt, type)] %>%
    stringr::str_replace_all(type, "") %>%
    stringr::str_replace_all("\\(%\\)", "") %>%
    stringr::str_split("\\s")
  if(length(spl) == 0) {
    return(NA)
  }
  shr <- as.numeric(spl[[1]])
  shr <- shr[!is.na(shr)]
  if(length(shr) > 0) {
    return(shr)
  } else {
    return(NA)
  }
}
