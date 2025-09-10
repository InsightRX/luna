#' Create a model object from the model code and dataset stored as a list 
#' object.
#' 
#' @param model_obj list object with entries `code` and `dataset`
#' 
create_pharmpy_model_from_list <- function(model_obj) {
  ## Pharmpy bug: datainfo not updated when using pharmar::set_dataset()
  ## So need to make sure the dataset is on file when loading the model
  code <- model_obj$code
  tmpfile <- tempfile()
  write.csv(model_obj$dataset, tmpfile, quote=F, row.names=F)
  code <- stringr::str_replace(
    code,
    "\\$DATA ([\\/a-zA-Z0-9\\.]*)",
    paste0("$DATA ", tmpfile)
  )
  model <- pharmr::read_model_from_string(code)
}
