test_that("luna lists available functions with descriptions", {
  expect_output(res <- luna(), "Available functions")
  expect_true(any(grepl("luna_run.*Run a model", res)))
  expect_true(any(grepl("luna_list.*List the models", res)))
})
