test_that("luna_tables reads model and gets tables from run folder", {
  folder <- local_luna_project()
  mock_tables <- mockery::mock(list(tab = fake_nm_table()))
  mockery::stub(luna_tables, "pharmr::read_model", "model_obj")
  mockery::stub(luna_tables, "get_tables_from_fit", mock_tables)
  expect_message(res <- luna_tables("run1"), "Looking for output tables")
  expect_equal(res, list(tab = fake_nm_table()))
  args <- mockery::mock_args(mock_tables)[[1]]
  expect_equal(args[[1]], "model_obj")
  expect_equal(args$path, file.path(folder, "run1"))
})

test_that("luna_tables uses current folder when no cache is available", {
  local_no_luna_cache()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  write_nm_model("run1.mod")
  mock_tables <- mockery::mock(list())
  mockery::stub(luna_tables, "pharmr::read_model", "model_obj")
  mockery::stub(luna_tables, "get_tables_from_fit", mock_tables)
  luna_tables(1, verbose = FALSE)
  expect_equal(mockery::mock_args(mock_tables)[[1]]$path, file.path(".", "run1"))
})

test_that("luna_tables aborts when model file is missing", {
  folder <- local_luna_project()
  expect_error(luna_tables("run9", verbose = FALSE), "Model file not found")
})
