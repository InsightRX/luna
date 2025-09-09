test_that("get_shrinkage_values extracts correct values", {
  # Test case with valid NONMEM output format
  test_txt <- c(
    "Some other text",
    "ETASHRINKSD(%) 2.345E+01 3.456E+01",
    "More text"
  )

  result <- get_shrinkage_values(test_txt, "ETASHRINKSD")
  expect_equal(result, c(23.45, 34.56))

  # Test case with no matches
  test_txt_empty <- c(
    "Some other text",
    "No shrinkage here"
  )
  result_empty <- get_shrinkage_values(test_txt_empty, "ETASHRINKSD")
  expect_equal(result_empty, NA)

  # Test different shrinkage types
  test_txt_eps <- "EPSSHRINKSD(%) 1.234E+01 2.345E+01"
  result_eps <- get_shrinkage_values(test_txt_eps, "EPSSHRINKSD")
  expect_equal(result_eps, c(12.34, 23.45))
})

test_that("get_shrinkage_summary returns correct structure", {
  # Create a temporary file with test content
  test_content <- c(
    "Some header",
    "ETASHRINKSD(%) 2.345E+01 3.456E+01",
    "EBVSHRINKSD(%) 1.234E+01 2.345E+01",
    "EPSSHRINKSD(%) 3.456E+01 4.567E+01"
  )
  temp_file <- tempfile(fileext = ".lst")
  writeLines(test_content, temp_file)

  # Test without fit object
  result <- get_shrinkage_summary(path = temp_file)
  expect_type(result, "list")
  expect_named(result, c("eta", "ebv", "eps"))
  expect_equal(result$eta, c(ETA_1 = 23.45, ETA_2 = 34.56))
  expect_equal(result$ebv, c(ETA_1 = 12.34, ETA_2 = 23.45))
  expect_equal(result$eps, c(34.56, 45.67))

  # Test with NULL path
  null_result <- get_shrinkage_summary(path = NULL)
  expect_equal(null_result, list())

  # Clean up
  unlink(temp_file)
})

