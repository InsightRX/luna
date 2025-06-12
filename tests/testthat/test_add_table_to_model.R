test_that("add_table_to_model adds table correctly", {
  # Create a mock model object
  mock_model <- list(
    code = "$PROBLEM test\n$INPUT ID TIME DV\n$DATA test.csv",
    tables = data.frame(file = "existing.txt")
  )
  class(mock_model) <- c("list", "pharmpy.model.external.nonmem.model.Model")
  
  # Mock the dependencies
  mockery::stub(add_table_to_model, "get_tables_in_model", function(model) c("existing.txt"))
  mockery::stub(add_table_to_model, "pharmr::read_model_from_string", function(code) {
    list(code = code)
  })

  # Test basic functionality
  result <- add_table_to_model(
    model = mock_model,
    variables = c("ID", "CL", "V"),
    firstonly = FALSE,
    file = "patab"
  )
  
  expected_addition <- "\\n\\$TABLE\\n  ID CL V\\n  NOAPPEND NOPRINT\\n  FILE=patab\\n\\n"
  expect_true(grepl(expected_addition, result$code))
  
  # Test with firstonly = TRUE
  result <- add_table_to_model(
    model = mock_model,
    variables = c("ID", "CL", "KA"),
    firstonly = TRUE,
    file = "patab"
  )
  
  expected_addition <- "\\n\\$TABLE\\n  ID CL KA\\n  FIRSTONLY\\n  NOAPPEND NOPRINT\\n  FILE=patab\\n\\n"
  expect_true(grepl(expected_addition, result$code))
})

test_that("add_table_to_model warns on duplicate file", {
  # Create a mock model object
  mock_model <- list(
    code = "$PROBLEM test\n$INPUT ID TIME DV\n$DATA test.csv $TABLE ID TIME FILE=patab",
    tables = data.frame(file = "patab")
  )
  class(mock_model) <- c("list", "pharmpy.model.external.nonmem.model.Model")
  
  # Mock get_tables_in_model to return our test file
  mockery::stub(add_table_to_model, "get_tables_in_model", function(model) c("patab"))
  mockery::stub(add_table_to_model, "pharmr::read_model_from_string", function(code) {
    list(code = code)
  })
  
  # Test warning is issued for duplicate file
  expect_warning(
    add_table_to_model(
      model = mock_model,
      variables = c("ID", "CL", "V"),
      firstonly = FALSE,
      file = "patab"
    ),
    "Table file already in a \\$TABLE record in model"
  )
})

test_that("add_table_to_model handles empty variables", {
  # Create a mock model object
  mock_model <- list(
    code = "$PROBLEM test\n$INPUT ID TIME DV\n$DATA test.csv",
    tables = data.frame(file = character(0))
  )
  class(mock_model) <- c("list", "pharmpy.model.external.nonmem.model.Model")
  
  # Mock the dependencies
  mockery::stub(add_table_to_model, "get_tables_in_model", function(model) character(0))
  mockery::stub(add_table_to_model, "pharmr::read_model_from_string", function(code) {
    list(code = code)
  })
  
  # Test with empty variables vector
  expect_warning(
    result <- add_table_to_model(
      model = mock_model,
      variables = character(0),
      firstonly = FALSE,
      file = "patab"
    )
  )
  expect_equal(result$code, mock_model$code)
  
})

