test_that("luna_diff aborts without luna cache", {
  local_no_luna_cache()
  expect_error(luna_diff("run2"), "Luna cache not found")
})

test_that("luna_diff uses reference from YAML", {
  dir <- local_luna_project()
  mockery::stub(luna_diff, "diffr::diffr", function(a, b) list(a, b))
  expect_message(out <- luna_diff("run2"), "Using run1 as reference model")
  expect_equal(out, list(file.path(dir, "run2.mod"), file.path(dir, "run1.mod")))
})

test_that("luna_diff accepts explicit reference and numeric ids", {
  dir <- local_luna_project()
  mockery::stub(luna_diff, "diffr::diffr", function(a, b) list(a, b))
  out <- luna_diff(1, reference = 2)
  expect_equal(out, list(file.path(dir, "run1.mod"), file.path(dir, "run2.mod")))
})

test_that("luna_diff uses explicit folder and <id>/run.mod", {
  local_luna_project()
  other <- withr::local_tempdir()
  dir.create(file.path(other, "run1"))
  write_nm_model(file.path(other, "run1", "run.mod"))
  write_nm_model(file.path(other, "run2.mod"))
  mockery::stub(luna_diff, "diffr::diffr", function(a, b) list(a, b))
  out <- luna_diff("run2", reference = "run1", folder = other)
  expect_equal(out, list(file.path(other, "run2.mod"), file.path(other, "run1", "run.mod")))
})

test_that("luna_diff aborts when run not in YAML", {
  local_luna_project()
  expect_error(luna_diff("run9"), "Cannot find run in YAML")
})

test_that("luna_diff aborts when run has no reference", {
  local_luna_project()
  expect_error(luna_diff("run1", verbose = FALSE), "No reference specified for run1")
})
