test_that("get_status returns 'not run' when no result file exists", {
  dir <- create_ferx_fixture(include_result = FALSE)

  mockery::stub(get_status, "find_file_with_fallback", NULL)

  status <- get_status(id = "run1", folder = dir)
  expect_equal(status, "not run")
})

test_that("get_status returns 'finished' when .fitrx exists and converged", {
  dir <- create_ferx_fixture(include_result = TRUE)

  mockery::stub(get_status, "find_file_with_fallback", NULL)
  mockery::stub(get_status, "ferx::ferx_load_fit", readRDS)

  status <- get_status(id = "run1", folder = dir)
  expect_equal(status, "finished")
})

test_that("get_status returns 'failed' when .fitrx exists but not converged", {
  dir <- create_ferx_fixture(include_result = FALSE)
  # Save a non-converged result
  fit <- fake_ferx_fit(converged = FALSE)
  saveRDS(fit, file.path(dir, "run1.fitrx"))

  mockery::stub(get_status, "find_file_with_fallback", NULL)
  mockery::stub(get_status, "ferx::ferx_load_fit", readRDS)

  status <- get_status(id = "run1", folder = dir)
  expect_equal(status, "failed")
})
