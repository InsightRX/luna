#' Get pharmpy configuration, as an R object (list)
#'
#' @export
#'
#' @returns a list object
#'
get_pharmpy_conf <- function() {

  ## Get path to config file
  pharmpy_conf <- pharmr::get_config_path()
  pharmpy_stop_msg <- "Cannot find Pharmpy configuration file. Please check your Pharmpy / pharmr installation."
  if(is.null(pharmpy_conf)) {
    cli::cli_abort(pharmpy_stop_msg)
  }
  if(!file.exists(pharmpy_conf)) {
    cli::cli_abort(pharmpy_stop_msg)
  }

  ## Read / check config file
  suppressWarnings({ # may throw warning about incomplete final line
    ini <- read_ini(pharmpy_conf)
  })
  if(!inherits(ini, "list")) {
    cli::cli_abort("Pharmpy configuration could not be interpreted. Please check your configuration file at {pharmpy_conf}")
  }

  ini
}

#' Helper function for read_ini
extract <- function(regexp, x) {
  regmatches(x, regexec(regexp, x))[[1]][2]
}

#' Read ini file core function
#'
read_ini <- function(fn) {
  blank = "^\\s*$"
  header = "^\\[(.*)\\]$"
  key_value = "^.*=.*$"
  lines = readLines(fn)
  ini <- list()
  for (l in lines) {
    if (grepl(blank, l)) next
    if (grepl(header, l)) {
      section = extract(header, l)
      ini[[section]] = list()
    }
    if (grepl(key_value, l)) {
      kv = strsplit(l, "\\s*=\\s*")[[1]]
      ini[[section]][[kv[1]]] = kv[2]
    }
  }
  ini
}
