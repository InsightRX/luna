test_that("update_cache records ferx model and result timestamps", {
  dir <- create_ferx_fixture()

  stored <- list()
  mock_cache <- list(
    set = function(key, value) { stored[[key]] <<- value },
    get = function(key) stored[[key]]
  )

  mock_project <- list(
    metadata = list(name = "test", folder = dir),
    yaml = list()
  )

  assign(".luna_cache", mock_cache, envir = rlang::global_env())
  withr::defer(rm(".luna_cache", envir = rlang::global_env()))

  mockery::stub(update_cache, "is_luna_cache_available", TRUE)

  update_cache(project = mock_project, verbose = FALSE)

  timestamps <- stored[["timestamps"]]

  expect_true("run1" %in% names(timestamps$models))
  expect_true("run1" %in% names(timestamps$results))
  expect_false(is.null(timestamps$models$run1))
  expect_false(is.null(timestamps$results$run1))
})

test_that("update_cache records NA timestamp when -fit.rds missing", {
  dir <- create_ferx_fixture(include_result = FALSE)

  stored <- list()
  mock_cache <- list(
    set = function(key, value) { stored[[key]] <<- value },
    get = function(key) stored[[key]]
  )

  mock_project <- list(
    metadata = list(name = "test", folder = dir),
    yaml = list()
  )

  assign(".luna_cache", mock_cache, envir = rlang::global_env())
  withr::defer(rm(".luna_cache", envir = rlang::global_env()))

  mockery::stub(update_cache, "is_luna_cache_available", TRUE)

  update_cache(project = mock_project, verbose = FALSE)

  timestamps <- stored[["timestamps"]]

  expect_true("run1" %in% names(timestamps$models))
  expect_true("run1" %in% names(timestamps$results))
})
