test_that("luna_xpose aborts when xpose is not installed", {
  mockery::stub(luna_xpose, "rlang::is_installed", FALSE)
  expect_error(luna_xpose("run1"), "not installed")
})

test_that("luna_xpose reads output file into xpose database", {
  folder <- local_luna_project()
  file.create(file.path(folder, "run1.lst"))
  mock_xp <- mockery::mock("xpdb")
  mockery::stub(luna_xpose, "rlang::is_installed", TRUE)
  mockery::stub(luna_xpose, "xpose::xpose_data", mock_xp)
  expect_message(res <- luna_xpose("run1"), "xpose package detected")
  expect_equal(res, "xpdb")
  expect_equal(mockery::mock_args(mock_xp)[[1]]$file, file.path(folder, "run1.lst"))
})

test_that("luna_xpose uses current folder when no cache is available", {
  local_no_luna_cache()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("run1")
  file.create(file.path("run1", "run.lst"))
  mock_xp <- mockery::mock("xpdb")
  mockery::stub(luna_xpose, "rlang::is_installed", TRUE)
  mockery::stub(luna_xpose, "xpose::xpose_data", mock_xp)
  luna_xpose(1, verbose = FALSE)
  expect_equal(mockery::mock_args(mock_xp)[[1]]$file, file.path(".", "run1", "run.lst"))
})
