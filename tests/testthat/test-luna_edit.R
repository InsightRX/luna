test_that("luna_edit opens model from active project", {
  dir <- local_luna_project()
  opened <- NULL
  mockery::stub(luna_edit, "file.edit", function(f) opened <<- f)
  expect_message(luna_edit(1), "Opening file")
  expect_equal(opened, file.path(dir, "run1.mod"))
})

test_that("luna_edit uses current folder without cache, and aborts on missing file", {
  dir <- local_luna_project(load = FALSE)
  withr::local_dir(dir)
  opened <- NULL
  mockery::stub(luna_edit, "file.edit", function(f) opened <<- f)
  luna_edit("run2", verbose = FALSE)
  expect_equal(opened, file.path(".", "run2.mod"))
  expect_error(luna_edit("run9", folder = dir), "Cannot find file")
})

test_that("luna_edit_project opens project yaml", {
  dir <- local_luna_project()
  opened <- NULL
  mockery::stub(luna_edit_project, "file.edit", function(f) opened <<- f)
  suppressMessages(luna_edit_project())
  expect_equal(opened, file.path(dir, "testproj.yaml"))
})

test_that("luna_edit_project aborts without cache or yaml", {
  local_no_luna_cache()
  mockery::stub(luna_edit_project, "file.edit", function(...) invisible(NULL))
  expect_error(luna_edit_project(), "cache not found")
  dir <- local_luna_project()
  file.remove(file.path(dir, "testproj.yaml"))
  expect_error(suppressMessages(luna_edit_project()), "Cannot find file project YAML")
})
