test_that("get_all_results extracts ferx fit data from .fitrx", {
  dir <- create_ferx_fixture()
  model_file <- file.path(dir, "run1.ferx")

  mockery::stub(get_all_results, "ferx::ferx_load_fit", readRDS)

  result <- get_all_results(model_file)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$model_file, model_file)
  expect_equal(result$ofv, 1234.56)
  expect_true(result$minimization_successful)
  expect_true(result$covstep_successful)
  expect_equal(result$runtime_total, 3.7)
  expect_equal(result$function_evaluations, 42L)
})

test_that("get_all_results returns model_file only when no .fitrx", {
  dir <- create_ferx_fixture(include_result = FALSE)
  model_file <- file.path(dir, "run1.ferx")

  result <- get_all_results(model_file)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$model_file, model_file)
  # When no .fitrx exists, only model_file is returned (no ofv column)
  expect_false("ofv" %in% names(result))
})

test_that("get_all_results handles NULL cov_matrix", {
  dir <- create_ferx_fixture(include_result = FALSE)
  fit <- fake_ferx_fit(cov_matrix = NULL)
  saveRDS(fit, file.path(dir, "run1.fitrx"))
  model_file <- file.path(dir, "run1.ferx")

  mockery::stub(get_all_results, "ferx::ferx_load_fit", readRDS)

  result <- get_all_results(model_file)

  expect_false(result$covstep_successful)
})
