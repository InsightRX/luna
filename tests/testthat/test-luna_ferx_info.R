test_that("luna_ferx_info errors when result file missing", {
  dir <- create_ferx_fixture(include_result = FALSE)

  mockery::stub(luna_ferx_info, "ferx::ferx_load_fit", readRDS)

  expect_error(
    luna_ferx_info(id = "run1", folder = dir),
    "No ferx results found"
  )
})

test_that("luna_ferx_info reads result and returns invisibly", {
  dir <- create_ferx_fixture()

  mockery::stub(luna_ferx_info, "ferx::ferx_load_fit", readRDS)

  result <- withVisible(luna_ferx_info(id = "run1", folder = dir))

  expect_false(result$visible)
  expect_equal(result$value$ofv, 1234.56)
  expect_true(result$value$converged)
  expect_equal(result$value$method, "foce")
})

test_that("luna_ferx_info works without optional aic/bic fields", {
  dir <- create_ferx_fixture(include_result = FALSE)

  minimal_fit <- list(
    ofv = 999.0,
    converged = FALSE,
    method = "foce"
  )
  saveRDS(minimal_fit, file.path(dir, "run1.fitrx"))

  # Should not error even without aic, bic, n_iterations, wall_time_secs
  mockery::stub(luna_ferx_info, "ferx::ferx_load_fit", readRDS)
  result <- luna_ferx_info(id = "run1", folder = dir)
  expect_equal(result$ofv, 999.0)
  expect_false(result$converged)
})

test_that("luna_ferx_info validates numeric id", {
  dir <- create_ferx_fixture()

  mockery::stub(luna_ferx_info, "ferx::ferx_load_fit", readRDS)

  # Numeric id should be converted to "run1"
  result <- luna_ferx_info(id = 1, folder = dir)
  expect_equal(result$ofv, 1234.56)
})
