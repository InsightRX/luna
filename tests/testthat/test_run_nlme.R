library(mockery)

## TODO: needs tests for main run_nlme function

test_that("get_new_run_number works correctly", {
  # Create temporary directory for testing
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Test 1: Empty directory should return 1
  expect_equal(get_new_run_number(temp_dir), 1)
  
  # Test 2: With existing run folders
  dir.create(file.path(temp_dir, "run1"))
  dir.create(file.path(temp_dir, "run2"))
  expect_equal(get_new_run_number(temp_dir), 3)
  
  # Test 3: Non-sequential numbers
  unlink(file.path(temp_dir, "run2"))
  dir.create(file.path(temp_dir, "run5"))
  expect_equal(get_new_run_number(temp_dir), 6)
  
  # Test 4: With non-run folders present
  dir.create(file.path(temp_dir, "other_folder"))
  expect_equal(get_new_run_number(temp_dir), 6)
  
  # Test 5: With invalid run folder names
  dir.create(file.path(temp_dir, "runA"))
  dir.create(file.path(temp_dir, "run"))
  expect_equal(get_new_run_number(temp_dir), 6)
})

test_that("change_nonmem_dataset handles different input formats correctly", {
  # Test single-line string input
  model_code_single <- "$PROB TEST\n$DATA old_data.csv IGNORE=@\n$INPUT ID TIME DV"
  result1 <- change_nonmem_dataset(model_code_single, "new_data.csv")
  expect_match(result1, "\\$DATA new_data\\.csv IGNORE=@")
  
  # Test vector input
  model_code_vector <- c("$PROB TEST", "$DATA old_data.csv IGNORE=@", "$INPUT ID TIME DV")
  result2 <- change_nonmem_dataset(model_code_vector, "new_data.csv")
  expect_match(result2, "\\$DATA new_data\\.csv IGNORE=@")
  
  # Test with multiple options after dataset
  model_code <- "$PROB TEST\n$DATA old_data.csv IGNORE=@ ACCEPT=(DV.GT.0)\n$INPUT ID TIME DV"
  result3 <- change_nonmem_dataset(model_code, "new_data.csv")
  expect_match(result3, "\\$DATA new_data\\.csv IGNORE=@ ACCEPT=\\(DV\\.GT\\.0\\)")
})

test_that("change_nonmem_dataset handles errors appropriately", {
  # Test missing $DATA line
  model_code_no_data <- "$PROB TEST\n$INPUT ID TIME DV"
  expect_error(
    change_nonmem_dataset(model_code_no_data, "new_data.csv"),
    "No \\$DATA line found in the model file"
  )
})

test_that("change_nonmem_dataset preserves whitespace and formatting", {
  # Test with extra whitespace
  model_code <- "$PROB TEST\n$DATA    old_data.csv    IGNORE=@   \n$INPUT ID TIME DV"
  result <- change_nonmem_dataset(model_code, "new_data.csv")
  expect_match(result, "\\$DATA new_data\\.csv IGNORE=@")
  
  # Test with tabs
  model_code_tabs <- "$PROB TEST\n$DATA\told_data.csv\tIGNORE=@\n$INPUT ID TIME DV"
  result <- change_nonmem_dataset(model_code_tabs, "new_data.csv")
  expect_match(result, "\\$DATA new_data\\.csv IGNORE=@")
})
