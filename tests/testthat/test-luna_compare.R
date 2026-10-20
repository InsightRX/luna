test_that("luna_compare gets info for each run and compares fits", {
  mock_info <- mockery::mock("fit1", "fit2")
  mock_compare <- mockery::mock("comparison")
  mockery::stub(luna_compare, "luna_info", mock_info)
  mockery::stub(luna_compare, "pharmr.extra::compare_nlme_fit", mock_compare)
  res <- luna_compare("run1", "run2", folder = "/proj", return_object = TRUE)
  expect_equal(res, "comparison")
  expect_equal(mockery::mock_args(mock_info)[[1]], list("run1", folder = "/proj"))
  args <- mockery::mock_args(mock_compare)[[1]]
  expect_equal(args[[1]], list(run1 = "fit1", run2 = "fit2"))
  expect_true(args$return_object)
})

test_that("luna_compare accepts runs as a vector", {
  mock_info <- mockery::mock("fit1", "fit2", "fit3")
  mock_compare <- mockery::mock(NULL)
  mockery::stub(luna_compare, "luna_info", mock_info)
  mockery::stub(luna_compare, "pharmr.extra::compare_nlme_fit", mock_compare)
  luna_compare(c("run1", "run2", "run3"))
  mockery::expect_called(mock_info, 3)
  args <- mockery::mock_args(mock_compare)[[1]]
  expect_named(args[[1]], c("run1", "run2", "run3"))
  expect_false(args$return_object)
})
