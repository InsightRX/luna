test_that("luna_load_project creates cache with project and timestamps", {
  dir <- local_luna_project()

  expect_true(is_luna_cache_available())
  project <- .luna_cache$get("project")
  expect_s3_class(project, "luna.project")
  expect_equal(project$metadata$name, "testproj")
  expect_equal(project$metadata$folder, dir)
  expect_equal(.luna_cache$get("name"), "testproj")
  expect_true(dir.exists(file.path(dir, ".luna.cache.testproj")))
  expect_setequal(names(.luna_cache$get("timestamps")$models), c("run1", "run2"))
})

test_that("luna_load_project detects project name from single yaml file", {
  dir <- local_luna_project(load = FALSE)
  expect_message(
    luna_load_project(folder = dir, verbose = FALSE),
    "Found luna project YAML file for project: testproj"
  )
  expect_equal(.luna_cache$get("name"), "testproj")
})

test_that("luna_load_project aborts when no or multiple yaml files found", {
  local_no_luna_cache()
  empty <- withr::local_tempdir()
  expect_error(luna_load_project(folder = empty), "No YAML files")
  dir <- local_luna_project(load = FALSE)
  writeLines("project: {}", file.path(dir, "other.yaml"))
  expect_error(luna_load_project(folder = dir), "Multiple YAML files")
})

test_that("luna_load_project creates cache when none exists on disk", {
  dir <- local_luna_project(load = FALSE)
  expect_message(
    luna_load_project(name = "testproj", folder = dir),
    "Creating luna project cache"
  )
  expect_true(dir.exists(file.path(dir, ".luna.cache.testproj")))
})

test_that("luna_load_project updates existing cache without reloading", {
  dir <- local_luna_project()
  msgs <- testthat::capture_messages(
    luna_load_project(name = "testproj", folder = dir)
  )
  expect_true(any(grepl("Updating luna project cache", msgs)))
  expect_false(any(grepl("Reloading", msgs)))
  expect_true(any(grepl("Active project: testproj", msgs)))
})

test_that("luna_load_project reloads disk cache when not in memory", {
  dir <- local_luna_project()
  rm(".luna_cache", envir = rlang::global_env())
  expect_message(
    luna_load_project(name = "testproj", folder = dir),
    "Reloading luna project cache"
  )
  expect_true(is_luna_cache_available())
  expect_equal(.luna_cache$get("folder"), dir)
})

test_that("luna_load_project switches cache between projects", {
  dir_a <- local_luna_project(name = "projA")
  dir_b <- local_luna_project(name = "projB")
  expect_equal(.luna_cache$get("name"), "projB")
  expect_message(
    luna_load_project(name = "projA", folder = dir_a),
    "Reloading luna project cache"
  )
  expect_equal(.luna_cache$get("name"), "projA")
  expect_equal(.luna_cache$get("project")$metadata$folder, dir_a)
  expect_message(
    luna_load_project(name = "projB", folder = dir_b),
    "Reloading luna project cache"
  )
  expect_equal(.luna_cache$get("name"), "projB")
  expect_equal(.luna_cache$get("folder"), dir_b)
})

test_that("luna_sync reloads the project", {
  dir <- local_luna_project(load = FALSE)
  luna_sync(name = "testproj", folder = dir, verbose = FALSE)
  expect_equal(.luna_cache$get("name"), "testproj")
})
