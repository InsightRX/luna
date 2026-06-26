#' Create a temporary ferx project fixture for testing
#'
#' Sets up a temp directory with a .ferx model file, a fake .fitrx result,
#' a minimal CSV data file, and a busulfan.yaml project config.
#'
#' @param id Character model id (default "run1")
#' @param include_result Logical; if TRUE, create a fake .fitrx (default TRUE)
#' @return Path to the temp project directory
create_ferx_fixture <- function(id = "run1", include_result = TRUE) {
  dir <- withr::local_tempdir(pattern = "ferx_test_", .local_envir = parent.frame())


  # .ferx model file
  writeLines(c(
    "; test model",
    "[parameters]",
    "  theta TVCL(10.0, 0.1, 100.0)",
    "  omega ETA_CL ~ 0.10",
    "  sigma PROP_ERR ~ 0.022",
    "[individual_parameters]",
    "  CL = TVCL * exp(ETA_CL)",
    "[structural_model]",
    "  pk one_cpt_iv(cl=CL, v=55)",
    "[error_model]",
    "  DV ~ proportional(PROP_ERR)",
    "[fit_options]",
    "  method     = foce",
    "  covariance = true"
  ), file.path(dir, paste0(id, ".ferx")))

  # Minimal CSV data file
  writeLines(c(
    "ID,TIME,AMT,DV,MDV,RATE",
    "1,0,280,0,1,93.33",
    "1,3,0,4.2,0,0",
    "1,6,0,2.1,0,0"
  ), file.path(dir, "data.csv"))

  # Fake ferx_fit result
  if (include_result) {
    fake_fit <- list(
      ofv = 1234.56,
      converged = TRUE,
      method = "foce",
      aic = 1244.56,
      bic = 1260.12,
      n_iterations = 42L,
      wall_time_secs = 3.7,
      cov_matrix = matrix(c(0.01, 0.001, 0.001, 0.02), nrow = 2)
    )
    saveRDS(fake_fit, file.path(dir, paste0(id, ".fitrx")))
  }

  # Project YAML
  yaml::write_yaml(list(
    project = list(
      description = "test project",
      config = list(
        tools = list(
          modelfit = list(method = "ferx", data = "data.csv")
        )
      ),
      runs = list(
        list(id = id, description = "test run")
      )
    )
  ), file.path(dir, "busulfan.yaml"))

  dir
}

#' Create a fake ferx_fit object
#'
#' @param ... Override any default fields
#' @return A list mimicking a ferx_fit result
fake_ferx_fit <- function(...) {
  defaults <- list(
    ofv = 1234.56,
    converged = TRUE,
    method = "foce",
    aic = 1244.56,
    bic = 1260.12,
    n_iterations = 42L,
    wall_time_secs = 3.7,
    cov_matrix = matrix(c(0.01, 0.001, 0.001, 0.02), nrow = 2)
  )
  overrides <- list(...)
  modifyList(defaults, overrides)
}
