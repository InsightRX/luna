#' Get estimation/simulation engine from pharmpy model
#' 
#' @param model pharmpy model
#' 
#' @export
get_tool_from_model <- function(model) {
  tool <- "nonmem"
  if(inherits(model, "pharmpy.model.external.nlmixr.model.Model")) {
    tool <- "nlmixr"
  }
  tool
}
