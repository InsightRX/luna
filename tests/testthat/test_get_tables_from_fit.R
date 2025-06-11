test_that("get_tables_from_fit handles models with tables correctly", {
  # Setup mock model and path
  mock_model <- list(
    name = "run001",
    code = "
      $TABLE ID TIME DV PRED IPRED
      FILE=sdtab001
      
      $TABLE CL V KA
      FILE=patab001
    "
  )
  temp_dir <- tempdir()
  model_dir <- file.path(temp_dir, "models", "run001")
  dir.create(model_dir, recursive = TRUE)
  
  # Create mock table files
  write.table(
    data.frame(ID = 1:3, TIME = 0:2, DV = 1:3, PRED = 2:4, IPRED = 1:3),
    file = file.path(model_dir, "sdtab001"),
    row.names = FALSE,
    quote = FALSE,
    sep = " "
  )
  
  write.table(
    data.frame(CL = c(0.1, 0.2, 0.3), V = c(1, 2, 3), KA = c(0.5, 0.6, 0.7)),
    file = file.path(model_dir, "patab001"),
    row.names = FALSE,
    quote = FALSE,
    sep = " "
  )
  
  # Test the function
  result <- get_tables_from_fit(mock_model, model_dir)
  
  # Assertions
  expect_type(result, "list")
  expect_named(result, c("sdtab001", "patab001"))
  expect_equal(ncol(result$sdtab001), 5)
  expect_equal(ncol(result$patab001), 3)
  expect_equal(nrow(result$sdtab001), 3)
  expect_equal(nrow(result$patab001), 3)
  
  # Cleanup
  unlink(model_dir, recursive = TRUE)
})

test_that("get_tables_from_fit handles models without tables correctly", {
  # Setup mock model without tables
  mock_model <- list(
    name = "run002",
    code = "$PROBLEM test\n$INPUT ID TIME\n$DATA data.csv"
  )
  
  result <- get_tables_from_fit(mock_model, tempdir())
  
  # Assertions
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("get_tables_from_fit handles missing files gracefully", {
  # Setup mock model with non-existent tables
  mock_model <- list(
    name = "run003",
    code = "
      $TABLE ID TIME
      FILE=missing001
    "
  )
  
  # Test should not error but return empty data
  expect_warning(
    result <- get_tables_from_fit(mock_model, tempdir()),
    NA
  )
  
  # Assertions
  expect_type(result, "list")
})

