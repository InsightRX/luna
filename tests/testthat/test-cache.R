test_that("is_luna_cache_available detects cache and can abort", {
  local_no_luna_cache()
  expect_false(is_luna_cache_available())
  expect_error(is_luna_cache_available(abort = TRUE), "cache not found")
  expect_error(luna_cache_get("name"), "cache not found")
})

test_that("luna_cache_get returns entries from active project", {
  dir <- local_luna_project()
  expect_true(is_luna_cache_available())
  expect_equal(luna_cache_get("name"), "testproj")
  expect_equal(luna_cache_get("folder"), dir)
})

test_that("create_cache stores project info verbosely", {
  dir <- local_luna_project(load = FALSE)
  project <- list(yaml = list(), metadata = list(name = "testproj", folder = dir))
  expect_message(create_cache(project, verbose = TRUE), "Reading project settings")
  expect_equal(.luna_cache$get("settings"), list())
  expect_equal(.luna_cache$get("project"), project)
  expect_true(dir.exists(file.path(dir, ".luna.cache.testproj")))
})

test_that("update_cache aborts without cache", {
  local_no_luna_cache()
  project <- list(metadata = list(name = "x", folder = tempdir()))
  expect_error(update_cache(project), "cache not found")
})

test_that("update_cache records NONMEM model and result timestamps", {
  dir <- local_luna_project()
  writeLines("results", file.path(dir, "run1.lst"))
  project <- .luna_cache$get("project")
  expect_message(update_cache(project, verbose = TRUE), "Updating timestamps")
  ts <- .luna_cache$get("timestamps")
  expect_setequal(names(ts$models), c("run1", "run2"))
  expect_match(ts$models$run1, "^\\d{4}-")
  expect_match(ts$results$run1, "^\\d{4}-")
  expect_equal(ts$results$run2, "")
})

test_that("update_cache records folder timestamps", {
  dir <- local_luna_project()
  dir.create(file.path(dir, "run1"))
  writeLines("x", file.path(dir, "run1", "out.txt"))
  withr::local_dir(dir)
  update_cache(.luna_cache$get("project"))
  ts <- .luna_cache$get("timestamps")
  expect_true("run1" %in% names(ts$folders))
  expect_match(ts$folders$run1, "^\\d{4}-")
})

test_that("update_cache records folder timestamps outside the project folder", {
  dir <- local_luna_project()
  dir.create(file.path(dir, "run1"))
  writeLines("x", file.path(dir, "run1", "out.txt"))
  withr::local_dir(tempdir())
  update_cache(.luna_cache$get("project"))
  ts <- .luna_cache$get("timestamps")
  expect_match(ts$folders$run1, "^\\d{4}-")
})

test_that("update_cache handles regex special characters in folder path", {
  local_luna_home()
  local_no_luna_cache()
  parent <- normalizePath(withr::local_tempdir())
  dir <- file.path(parent, "proj (v1.0)+")
  dir.create(dir)
  dir.create(file.path(dir, "run1"))
  writeLines("x", file.path(dir, "run1", "out.txt"))
  yaml::write_yaml(list(project = list(description = "d")), file.path(dir, "p.yaml"))
  suppressMessages(luna_load_project(name = "p", folder = dir, verbose = FALSE))
  ts <- .luna_cache$get("timestamps")
  expect_true("run1" %in% names(ts$folders))
  expect_match(ts$folders$run1, "^\\d{4}-")
})
