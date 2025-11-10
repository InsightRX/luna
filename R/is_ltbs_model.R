#' Is the residual error model "log-transform both-sides"?
#'
#' @inheritParams create_model
#'
#' @export
#'
is_ltbs_model <- function(model) {
  res <- model$internals$control_stream$get_error_record()
  if(!is.null(res) && inherits(res, "pharmpy.model.external.nonmem.records.code_record.CodeRecord")) {
    y <- res$statements$find_assignment("Y")
    if(!is.null(y) && inherits(y, "pharmpy.model.statements.Assignment")) {
      return(
        stringr::str_detect(
          y$to_dict()$expression,
          "log\\(.*?\\)"
        )
      )
    }
  }
  FALSE
}
