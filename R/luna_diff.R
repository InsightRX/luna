#' Show a diff between two models
#'
#' @inheritParams luna_run
#' @param id either a single `id` or a vector of exactly two model `id`s.
#' If specified as a single value, the `id` needs to have a `reference` model
#' specified to which it can be compared. If `id` is of length 2, then it will
#' just diff the models for the two `id`s.
#' @param reference id for reference model to compare to (optional)
#'
#' @export
#'
luna_diff <- function(
  id,
  reference = NULL,
  folder = NULL,
  verbose = TRUE
) {

  ## Checks
  is_luna_cache_available(abort = TRUE)
  if(is.null(folder)) {
    folder <- .luna_cache$get("project")$metadata$folder
  }

  ## Figure out which files to look at
  if(!is.null(reference)) {
    new_id <- validate_id(id)
    ref_id <- validate_id(reference)
  } else { # get reference from YAML, if available
    new_id <- validate_id(id)
    runs <- .luna_cache$get("project")$yaml$runs
    log_entry <- pluck_entry(runs, new_id)
    if(is.null(log_entry)) {
      cli::cli_abort("Cannot find run in YAML.")
    }
    ref_id <- log_entry$reference
    if(is.null(ref_id)) {
      cli::cli_abort("No reference specified for {new_id}")
    } else {
      if(verbose) {
        cli::cli_alert_info("Using {ref_id} as reference model.")
      }
    }
  }

  ## Load files
  model_file_new <- find_file_with_fallback(
    folder,
    filename = file.path(new_id, paste0("run", ".mod")),
    fallback = paste0(new_id, ".mod"),
    verbose = FALSE
  )
  model_file_ref <- find_file_with_fallback(
    folder,
    filename = file.path(ref_id, paste0("run", ".mod")),
    fallback = paste0(ref_id, ".mod"),
    verbose = FALSE
  )

  ## Generate diff
  diffr::diffr(
    model_file_new,
    model_file_ref
  )

}
