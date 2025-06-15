#' Add a tag to a run
#' Either add a tag or clear tags from a run
#'
#' @inheritParams luna_note
#' @param tag tag to add (character vector)
#'
#' @examples
#' \dontrun{
#' luna_tag("run1", "final") # adds a tag
#' luna_tag("run1", "intermediate", clear=TRUE) # first clears existing tags, then adds a tag
#' luna_tag("run1", clear=TRUE) # only clears existing tags
#' }
#'
#' @export
#'
luna_tag <- function(
  id,
  tag = NULL,
  clear = FALSE,
  force = TRUE,
  verbose = FALSE
) {

  is_luna_cache_available(abort = TRUE)

  luna_note(
    id,
    note = tag,
    clear = clear,
    force = force,
    verbose = verbose,
    element = "tag"
  )

}
