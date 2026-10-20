test_that("luna_info returns fit with info, run, tools and model attributes", {
  folder <- local_luna_project(runs = list(
    list(id = "run1", description = "base"),
    list(
      id = "run2", description = "alt", reference = "run1",
      tools = list(list(tool = "vpc"), list(tool = "bootstrap"))
    )
  ))
  file.create(file.path(folder, c("run1.lst", "run2.lst")))
  dir.create(file.path(folder, "run2", "vpc_dir1"), recursive = TRUE)
  mock_fit <- mockery::mock(list(ofv = 90), list(ofv = 100))
  mock_info <- mockery::mock(list(ofv = 90))
  mockery::stub(luna_info, "pharmr::read_modelfit_results", mock_fit)
  mockery::stub(luna_info, "get_fit_info", mock_info)
  mockery::stub(luna_info, "pharmr::read_model", "model_obj")
  res <- luna_info("run2")
  expect_equal(res$ofv, 90)
  expect_equal(attr(res, "info")$reference_run, "run1")
  expect_equal(attr(res, "info")$reference_ofv, 100)
  expect_equal(attr(res, "info")$dofv, -10)
  expect_equal(attr(res, "run")$id, "run2")
  expect_equal(attr(res, "tools"), list(vpc = "vpc_dir1"))
  expect_equal(attr(res, "model"), "model_obj")
  info_args <- mockery::mock_args(mock_info)[[1]]
  expect_equal(info_args$path, folder)
  expect_equal(info_args$output_file, file.path(folder, "run2.lst"))
  expect_equal(
    mockery::mock_args(mock_fit)[[2]]$path,
    file.path(folder, "run1.mod")
  )
})

test_that("luna_info skips reference info when run has no reference", {
  folder <- local_luna_project()
  file.create(file.path(folder, "run1.lst"))
  mock_fit <- mockery::mock(list(ofv = 90))
  mockery::stub(luna_info, "pharmr::read_modelfit_results", mock_fit)
  mockery::stub(luna_info, "get_fit_info", function(...) list())
  mockery::stub(luna_info, "pharmr::read_model", "model_obj")
  res <- luna_info(1, folder = folder)
  mockery::expect_called(mock_fit, 1)
  expect_null(attr(res, "info")$reference_run)
  expect_equal(attr(res, "tools"), list())
})

test_that("luna_info omits dofv when OFVs are not numeric", {
  folder <- local_luna_project()
  file.create(file.path(folder, c("run1.lst", "run2.lst")))
  mockery::stub(luna_info, "pharmr::read_modelfit_results", list(ofv = NULL))
  mockery::stub(luna_info, "get_fit_info", function(...) list())
  mockery::stub(luna_info, "pharmr::read_model", NULL)
  res <- luna_info("run2")
  expect_equal(attr(res, "info")$reference_run, "run1")
  expect_null(attr(res, "info")$dofv)
})
