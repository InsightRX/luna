test_that("get_status returns 'not run' without output", {
  dir <- withr::local_tempdir()
  expect_equal(get_status("run1", dir), "not run")
})

test_that("get_status finds <id>/run.lst", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "run1"))
  writeLines("lst", file.path(dir, "run1", "run.lst"))
  expect_equal(get_status("run1", dir), "finished")
})

test_that("get_status falls back to <id>.lst", {
  dir <- withr::local_tempdir()
  writeLines("lst", file.path(dir, "run1.lst"))
  expect_equal(get_status("run1", dir), "finished")
  expect_equal(get_status("run2", dir), "not run")
})
