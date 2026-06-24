test_that("luna_run_ferx errors when ferx package not installed", {
  dir <- create_ferx_fixture()
  config <- list(tools = list(modelfit = list(method = "ferx", data = "data.csv")))

  mockery::stub(luna_run_ferx, "requireNamespace", FALSE)

  expect_error(
    luna_run_ferx(id = "run1", folder = dir, config = config),
    "ferx"
  )
})

test_that("luna_run_ferx errors when model file missing", {
  dir <- create_ferx_fixture()
  config <- list(tools = list(modelfit = list(method = "ferx", data = "data.csv")))

  # Remove the .ferx file

  file.remove(file.path(dir, "run1.ferx"))

  mockery::stub(luna_run_ferx, "requireNamespace", TRUE)

  expect_error(
    luna_run_ferx(id = "run1", folder = dir, config = config),
    "not found"
  )
})

test_that("luna_run_ferx errors when data config is NULL", {
  dir <- create_ferx_fixture()
  config <- list(tools = list(modelfit = list(method = "ferx", data = NULL)))

  mockery::stub(luna_run_ferx, "requireNamespace", TRUE)

  expect_error(
    luna_run_ferx(id = "run1", folder = dir, config = config),
    "data file path"
  )
})

test_that("luna_run_ferx errors when data file not found", {
  dir <- create_ferx_fixture()
  config <- list(tools = list(modelfit = list(method = "ferx", data = "nonexistent.csv")))

  mockery::stub(luna_run_ferx, "requireNamespace", TRUE)

  expect_error(
    luna_run_ferx(id = "run1", folder = dir, config = config),
    "not found"
  )
})

test_that("luna_run_ferx resolves relative data path to folder", {
  dir <- create_ferx_fixture()
  config <- list(tools = list(modelfit = list(method = "ferx", data = "data.csv")))

  mock_fit <- fake_ferx_fit()
  mock_ferx_fit <- mockery::mock(mock_fit)
  mock_section <- mockery::mock(character(0))

  mockery::stub(luna_run_ferx, "requireNamespace", TRUE)
  mockery::stub(luna_run_ferx, "ferx::ferx_fit", mock_ferx_fit)
  mockery::stub(luna_run_ferx, "ferx::ferx_model_section", mock_section)
  mockery::stub(luna_run_ferx, "log_add", NULL)

  luna_run_ferx(id = "run1", folder = dir, config = config)

  # Verify ferx_fit was called with the resolved absolute data path
  call_args <- mockery::mock_args(mock_ferx_fit)[[1]]
  expect_equal(call_args$data, file.path(dir, "data.csv"))
})

test_that("luna_run_ferx uses absolute data path as-is", {
  dir <- create_ferx_fixture()
  abs_data <- file.path(dir, "data.csv")
  config <- list(tools = list(modelfit = list(method = "ferx", data = abs_data)))

  mock_fit <- fake_ferx_fit()
  mock_ferx_fit <- mockery::mock(mock_fit)
  mock_section <- mockery::mock(character(0))

  mockery::stub(luna_run_ferx, "requireNamespace", TRUE)
  mockery::stub(luna_run_ferx, "ferx::ferx_fit", mock_ferx_fit)
  mockery::stub(luna_run_ferx, "ferx::ferx_model_section", mock_section)
  mockery::stub(luna_run_ferx, "log_add", NULL)

  luna_run_ferx(id = "run1", folder = dir, config = config)

  call_args <- mockery::mock_args(mock_ferx_fit)[[1]]
  expect_equal(call_args$data, abs_data)
})

test_that("luna_run_ferx saves .rds and returns result invisibly", {
  dir <- create_ferx_fixture(include_result = FALSE)
  config <- list(tools = list(modelfit = list(method = "ferx", data = "data.csv")))

  mock_fit <- fake_ferx_fit()
  mock_ferx_fit <- mockery::mock(mock_fit)
  mock_section <- mockery::mock(character(0))

  mockery::stub(luna_run_ferx, "requireNamespace", TRUE)
  mockery::stub(luna_run_ferx, "ferx::ferx_fit", mock_ferx_fit)
  mockery::stub(luna_run_ferx, "ferx::ferx_model_section", mock_section)
  mockery::stub(luna_run_ferx, "log_add", NULL)

  result <- withVisible(luna_run_ferx(id = "run1", folder = dir, config = config))

  # Returns invisibly

  expect_false(result$visible)
  expect_equal(result$value$ofv, 1234.56)

  # .rds was saved
  rds_path <- file.path(dir, "run1-fit.rds")
  expect_true(file.exists(rds_path))
  saved <- readRDS(rds_path)
  expect_equal(saved$ofv, 1234.56)
})

test_that("luna_run_ferx extracts method from .ferx fit_options", {
  dir <- create_ferx_fixture(include_result = FALSE)
  config <- list(tools = list(modelfit = list(method = "ferx", data = "data.csv")))

  mock_fit <- fake_ferx_fit()
  mock_ferx_fit <- mockery::mock(mock_fit)
  mock_section <- mockery::mock(c("  method     = foce", "  covariance = true"))

  mockery::stub(luna_run_ferx, "requireNamespace", TRUE)
  mockery::stub(luna_run_ferx, "ferx::ferx_fit", mock_ferx_fit)
  mockery::stub(luna_run_ferx, "ferx::ferx_model_section", mock_section)
  mockery::stub(luna_run_ferx, "log_add", NULL)

  luna_run_ferx(id = "run1", folder = dir, config = config)

  call_args <- mockery::mock_args(mock_ferx_fit)[[1]]
  expect_equal(call_args$method, "foce")
})

test_that("luna_run_ferx reports non-convergence", {
  dir <- create_ferx_fixture(include_result = FALSE)
  config <- list(tools = list(modelfit = list(method = "ferx", data = "data.csv")))

  mock_fit <- fake_ferx_fit(converged = FALSE)
  mock_ferx_fit <- mockery::mock(mock_fit)
  mock_section <- mockery::mock(character(0))

  mockery::stub(luna_run_ferx, "requireNamespace", TRUE)
  mockery::stub(luna_run_ferx, "ferx::ferx_fit", mock_ferx_fit)
  mockery::stub(luna_run_ferx, "ferx::ferx_model_section", mock_section)
  mockery::stub(luna_run_ferx, "log_add", NULL)

  expect_message(
    luna_run_ferx(id = "run1", folder = dir, config = config),
    "did not converge"
  )
})
