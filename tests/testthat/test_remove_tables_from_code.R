test_that("remove_table_sections handles various cases correctly", {
  # Test 1: Basic case with single $TABLE
  basic_text <- "$INPUT\nsome input\n$TABLE\nsome table\n$ERROR\nsome error"
  expect_equal(
    remove_table_sections(basic_text),
    "$INPUT\nsome input\n$ERROR\nsome error"
  )

  # Test 2: Multiple $TABLE sections
  multiple_tables <- "$INPUT\ndata\n$TABLE\ntable1\n$PK\npk data\n$TABLE\ntable2\n$ERROR\nerror"
  expect_equal(
    remove_table_sections(multiple_tables),
    "$INPUT\ndata\n$PK\npk data\n$ERROR\nerror"
  )

  # Test 3: $TABLE at the end of text
  end_table <- "$INPUT\ndata\n$PK\npk data\n$TABLE\ntable data"
  expect_equal(
    remove_table_sections(end_table),
    "$INPUT\ndata\n$PK\npk data"
  )

  # Test 4: No $TABLE sections
  no_table <- "$INPUT\ndata\n$PK\npk data\n$ERROR\nerror"
  expect_equal(
    remove_table_sections(no_table),
    no_table
  )

  # Test 5: Empty string
  expect_equal(
    remove_table_sections(""),
    ""
  )

  # Test 6: Complex case with multiple newlines and spacing
  complex_text <- "$INPUT\ndata\n\n$TABLE \n  ID TIME DV\n  EVID\n\n$PK\npk\n\n$TABLE\nmore table\n\n$ERROR"
  expect_equal(
    remove_table_sections(complex_text),
    "$INPUT\ndata\n\n$PK\npk\n\n$ERROR"
  )

  # Test 7: Case with $TABLE-like content in other sections
  table_like <- "$INPUT\nTABLE1 TABLE2\n$PK\nTABLE_VAR=1\n$TABLE\nreal table\n$ERROR"
  expect_equal(
    remove_table_sections(table_like),
    "$INPUT\nTABLE1 TABLE2\n$PK\nTABLE_VAR=1\n$ERROR"
  )
})

# Additional test for verifying no $TABLE sections remain
test_that("no $TABLE sections remain after removal", {
  test_cases <- list(
    "$INPUT\n$TABLE\ndata\n$ERROR",
    "$INPUT\n$TABLE\ndata1\n$PK\n$TABLE\ndata2\n$ERROR",
    "$INPUT\n$TABLE\ndata\n",
    "$INPUT\n$TABLE\ndata1\n$TABLE\ndata2"
  )

  for (test_case in test_cases) {
    result <- remove_table_sections(test_case)
    expect_false(grepl("\\$TABLE", result),
                 info = "Found $TABLE section that should have been removed")
  }
})

test_that("remove_table_sections with file argument", {
  # Test 1: Basic case with single $TABLE
  basic_text <- "$INPUT\nsome input\n$TABLE\nsome table FILE=sdtab1 \n$TABLE\nsome table FILE=patab1\n$ERROR\nsome error"
  expect_equal(
    remove_table_sections(basic_text, file = "patab"),
    "$INPUT\nsome input\n$TABLE\nsome table FILE=sdtab1 \n1\n$ERROR\nsome error"
  )
})
