test_that("get_tables_in_model_code extracts table filenames correctly", {
  # Test case 1: Simple table
  model_simple <- list(
    code = "$TABLE FILE=sdtab001"
  )
  expect_equal(
    get_tables_in_model_code(model_simple),
    "sdtab001"
  )
  
  # Test case 2: Multiple tables
  model_multiple <- list(
    code = "$TABLE FILE=sdtab001 NOPRINT ONEHEADER\n$TABLE FILE=patab001"
  )
  expect_equal(
    get_tables_in_model_code(model_multiple),
    c("sdtab001", "patab001")
  )
  
  # Test case 3: Complex model with other sections
  model_complex <- list(
    code = "
      $PROBLEM Test model
      $INPUT ID TIME DV
      $DATA data.csv
      $TABLE FILE=sdtab001 NOPRINT
      $TABLE FILE=mytab002 NOAPPEND
      $TABLE FILE=cotab003
    "
  )
  expect_equal(
    get_tables_in_model_code(model_complex),
    c("sdtab001", "mytab002", "cotab003")
  )
  
  # Test case 4: No tables
  model_no_tables <- list(
    code = "$PROBLEM Test model\n$INPUT ID TIME DV\n$DATA data.csv"
  )
  expect_equal(
    get_tables_in_model_code(model_no_tables),
    character(0)
  )
})
