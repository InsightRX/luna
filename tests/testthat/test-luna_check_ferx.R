test_that("luna_check returns TRUE when .ferx file exists", {
  dir <- create_ferx_fixture()

  mock_project <- list(
    metadata = list(name = "test", folder = dir),
    yaml = list()
  )
  mock_cache <- list(
    get = function(key) mock_project,
    set = function(key, value) NULL
  )
  mock_config <- list(tools = list(modelfit = list(method = "ferx")))

  assign(".luna_cache", mock_cache, envir = rlang::global_env())
  withr::defer(rm(".luna_cache", envir = rlang::global_env()))

  mockery::stub(luna_check, "is_luna_cache_available", TRUE)
  mockery::stub(luna_check, "get_luna_config", mock_config)
  mockery::stub(luna_check, "luna_load_project", NULL)

  result <- luna_check(id = "run1", folder = dir)
  expect_true(result)
})

test_that("luna_check returns FALSE when .ferx file missing", {
  dir <- create_ferx_fixture()
  file.remove(file.path(dir, "run1.ferx"))

  mock_project <- list(
    metadata = list(name = "test", folder = dir),
    yaml = list()
  )
  mock_cache <- list(
    get = function(key) mock_project,
    set = function(key, value) NULL
  )
  mock_config <- list(tools = list(modelfit = list(method = "ferx")))

  assign(".luna_cache", mock_cache, envir = rlang::global_env())
  withr::defer(rm(".luna_cache", envir = rlang::global_env()))

  mockery::stub(luna_check, "is_luna_cache_available", TRUE)
  mockery::stub(luna_check, "get_luna_config", mock_config)
  mockery::stub(luna_check, "luna_load_project", NULL)

  result <- luna_check(id = "run1", folder = dir)
  expect_false(result)
})
