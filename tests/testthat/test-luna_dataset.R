test_that("luna_dataset returns dataset from model", {
  local_luna_project()
  ds <- data.frame(ID = 1:2)
  mock_read <- mockery::mock(list(dataset = ds))
  mockery::stub(luna_dataset, "pharmr::read_model", mock_read)
  expect_equal(luna_dataset(1), ds)
  expect_match(mockery::mock_args(mock_read)[[1]]$path, "run1.mod$")
})

test_that("luna_dataset aborts when model file is missing", {
  folder <- local_luna_project()
  expect_error(luna_dataset("run9", folder = folder), "Model file not found")
})

test_that("luna_dataset aborts when dataset is missing", {
  local_luna_project()
  mockery::stub(luna_dataset, "pharmr::read_model", list(dataset = NULL))
  expect_error(luna_dataset("run1"), "Data file not found")
})
