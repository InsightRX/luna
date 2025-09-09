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
  if(is.null(eta)) {
    cli::cli_alert_info("Cannot compute shrinkage.")
    return(list())
  }
  names(eta) <- eta_names[1:length(eta)]
  names(ebv) <- eta_names[1:length(eta)]
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
  idx <- grep(type, txt)
  if(length(idx) == 0) {
    return(NA)
  } else {
    if(length(idx) > 1) {
      idx <- idx[1]
    }
  }
  line <- txt[idx]
  ## Also check in next 2 lines for more values
  line1 <- gsub(" ", "", substr(txt[idx + 1], 1, 16))
  line2 <- gsub(" ", "", substr(txt[idx + 2], 1, 16))
  if(!is.na(line1) && line1 == "") {
    line <- paste0(line, txt[idx+1])
  }
  if(!is.na(line2) && line2 == "") {
    line <- paste0(line, txt[idx+2])
  }
  spl <- line |>
    stringr::str_replace_all(type, "") |>
    stringr::str_replace_all("\\(%\\)", "") |>
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
