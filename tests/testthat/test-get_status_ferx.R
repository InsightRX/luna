test_that("get_status returns 'not run' when no result file exists", {
  dir <- create_ferx_fixture(include_result = FALSE)

  mockery::stub(get_status, "find_file_with_fallback", NULL)

  status <- get_status(id = "run1", folder = dir)
  expect_equal(status, "not run")
})

test_that("get_status returns 'finished' when -fit.rds exists", {
  dir <- create_ferx_fixture(include_result = TRUE)

  mockery::stub(get_status, "find_file_with_fallback", NULL)

  status <- get_status(id = "run1", folder = dir)
  expect_equal(status, "finished")
})
