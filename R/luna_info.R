#' Show info for a model run
#'
#' @inheritParams luna_run
#'
#' @export
#'
luna_info <- function(
  id,
  folder = NULL
) {
  id <- validate_id(id)
  if(is.null(folder)) {
    folder <- .luna_cache$get("project")$metadata$folder
  }
  model_file <- find_file_with_fallback(
    folder,
    filename = file.path(id, paste0("run", ".mod")),
    fallback = paste0(id, ".mod"),
    verbose = FALSE
  )
  output_file <- find_file_with_fallback(
    folder,
    filename = file.path(id, paste0("run", ".lst")),
    fallback = paste0(id, ".lst"),
    verbose = FALSE
  )

  fit <- pharmr::read_modelfit_results(
    esttool = "nonmem",
    path = model_file
  )
  fit_info <- get_fit_info(
    fit,
    path = folder,
    output_file = output_file
  )
  runs <- .luna_cache$get("project")$yaml$runs
  run_info <- pluck_entry(runs, id)
  if(!is.null(run_info$reference)) {
    fit_ref <- pharmr::read_modelfit_results(
      esttool = "nonmem",
      path = file.path(folder, paste0(run_info$reference, ".mod"))
    )
    fit_info$reference_run <- run_info$reference
    fit_info$reference_ofv <- fit_ref$ofv
    if(is.numeric(fit_ref$ofv) && is.numeric(fit$ofv)) {
      fit_info$dofv <- fit$ofv - fit_ref$ofv
    }
  }
  attr(fit, "info") <- fit_info

  ## Attach tool info
  defined_tools <- unique(unlist(lapply(run_info$tools, function(x) x$tool)))
  tools_info <- get_tools_info(id, folder, defined_tools)
  attr(fit, "tools") <- tools_info

  ## attach model object
  model <- pharmr::read_model(
    path = model_file
  )
  attr(fit, "model") <- model

  fit
}
