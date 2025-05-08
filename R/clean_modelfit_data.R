#' Clean / check the dataset before passing to model fitting tool
#'
#' @inheritParams luna_run
#'
#' @export
clean_modelfit_data <- function(model, data = NULL) {

  tool <- ifelse(inherits(model, "pharmpy.model.external.nonmem.model.Model"), "nonmem", "nlmixr")

  if(is.null(data)) {
    data <- model$dataset
  }
  if(any(lapply(data, class) != "character")) {
    data <- data[,lapply(data, class) != "character"]

    if(tool != "nonmem") { ## nlmixr2 requires lower-case `cmt`
      data <- data %>%
        rename(cmt = CMT)
    }

    ## Save dataset
    dataset <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(data, dataset, quote=F, row.names=F)

    ## Update dataset in model
    model <- pharmr::set_dataset(
      model,
      path_or_df = dataset,
      datatype = "nonmem"
    )
  }
  model
}
