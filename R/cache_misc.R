#' Get element from active project
#' Just a convenience wrapper with error checking
#'
luna_cache_get <- function(
  entry = c("name", "folder")
) {
  has_cache <- is_luna_cache_available(abort = TRUE)
  .luna_cache$get(entry)
}

#' Check if luna cache is available / loaded
#'
is_luna_cache_available <- function(abort = FALSE) {
  is_cache_present <- ".luna_cache" %in% objects(all.names = TRUE, envir = rlang::global_env())
  if(!is_cache_present && abort) {
    cli::cli_abort("Luna cache not found, please first load or create a luna project.")
  }
  is_cache_present
}
