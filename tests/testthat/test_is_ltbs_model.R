# tests/testthat/test-is_ltbs_model.R

test_that("is_ltbs_model returns FALSE when control_stream is missing from model internals", {
  # Create a mock model without control_stream
  model <- list(
    internals = list(
      other_component = "some_value"
    )
  )
  
  # Expect a warning and FALSE return
  expect_message(
    result <- is_ltbs_model(model),
    "Check for LTBS not yet implemented for nlmixr2."
  )
  expect_false(result)
})

test_that("is_ltbs_model returns FALSE when model internals is missing", {
  # Create a mock model without internals
  model <- list(
    some_other_field = "value"
  )
  
  expect_message(
    result <- is_ltbs_model(model),
    "Check for LTBS not yet implemented for nlmixr2."
  )
  expect_false(result)
})

test_that("is_ltbs_model returns FALSE when get_error_record returns NULL", {
  # Create a mock model with control_stream that returns NULL
  mock_control_stream <- list(
    get_error_record = function() NULL
  )
  
  model <- list(
    internals = list(
      control_stream = mock_control_stream
    )
  )
  
  result <- is_ltbs_model(model)
  expect_false(result)
})

test_that("is_ltbs_model returns FALSE when error record is not a CodeRecord", {
  # Create a mock that returns something that's not a CodeRecord
  mock_control_stream <- list(
    get_error_record = function() "not_a_code_record"
  )
  
  model <- list(
    internals = list(
      control_stream = mock_control_stream
    )
  )
  
  result <- is_ltbs_model(model)
  expect_false(result)
})

test_that("is_ltbs_model returns FALSE when Y assignment is not found", {
  # Create a mock CodeRecord without Y assignment
  mock_statements <- list(
    find_assignment = function(var) {
      if (var == "Y") return(NULL)
      return("some_other_assignment")
    }
  )
  
  mock_error_record <- list(statements = mock_statements)
  class(mock_error_record) <- "pharmpy.model.external.nonmem.records.code_record.CodeRecord"
  
  mock_control_stream <- list(
    get_error_record = function() mock_error_record
  )
  
  model <- list(
    internals = list(
      control_stream = mock_control_stream
    )
  )
  
  result <- is_ltbs_model(model)
  expect_false(result)
})

test_that("is_ltbs_model returns FALSE when Y assignment is not an Assignment object", {
  # Create a mock where find_assignment returns something that's not an Assignment
  mock_statements <- list(
    find_assignment = function(var) {
      if (var == "Y") return("not_an_assignment")
      return(NULL)
    }
  )
  
  mock_error_record <- list(statements = mock_statements)
  class(mock_error_record) <- "pharmpy.model.external.nonmem.records.code_record.CodeRecord"
  
  mock_control_stream <- list(
    get_error_record = function() mock_error_record
  )
  
  model <- list(
    internals = list(
      control_stream = mock_control_stream
    )
  )
  
  result <- is_ltbs_model(model)
  expect_false(result)
})

test_that("is_ltbs_model returns TRUE when Y assignment contains log transformation", {
  # Create a mock Assignment with log transformation
  mock_assignment <- list(
    to_dict = function() {
      list(expression = "log(THETA(1) + ETA(1))")
    }
  )
  class(mock_assignment) <- "pharmpy.model.statements.Assignment"
  
  mock_statements <- list(
    find_assignment = function(var) {
      if (var == "Y") return(mock_assignment)
      return(NULL)
    }
  )
  
  mock_error_record <- list(statements = mock_statements)
  class(mock_error_record) <- "pharmpy.model.external.nonmem.records.code_record.CodeRecord"
  
  mock_control_stream <- list(
    get_error_record = function() mock_error_record
  )
  
  model <- list(
    internals = list(
      control_stream = mock_control_stream
    )
  )
  
  result <- is_ltbs_model(model)
  expect_true(result)
})

test_that("is_ltbs_model returns FALSE when Y assignment does not contain log transformation", {
  # Create a mock Assignment without log transformation
  mock_assignment <- list(
    to_dict = function() {
      list(expression = "THETA(1) + ETA(1)")
    }
  )
  class(mock_assignment) <- "pharmpy.model.statements.Assignment"
  
  mock_statements <- list(
    find_assignment = function(var) {
      if (var == "Y") return(mock_assignment)
      return(NULL)
    }
  )
  
  mock_error_record <- list(statements = mock_statements)
  class(mock_error_record) <- "pharmpy.model.external.nonmem.records.code_record.CodeRecord"
  
  mock_control_stream <- list(
    get_error_record = function() mock_error_record
  )
  
  model <- list(
    internals = list(
      control_stream = mock_control_stream
    )
  )
  
  result <- is_ltbs_model(model)
  expect_false(result)
})

test_that("is_ltbs_model detects various log expressions", {
  # Test different log expressions
  log_expressions <- c(
    "log(THETA(1))",
    "log(F + ETA(1))",
    "log(IPRED)",
    "Y = log(F) + EPS(1)"
  )
  
  for (expr in log_expressions) {
    mock_assignment <- list(
      to_dict = function() list(expression = expr)
    )
    class(mock_assignment) <- "pharmpy.model.statements.Assignment"
    
    mock_statements <- list(
      find_assignment = function(var) {
        if (var == "Y") return(mock_assignment)
        return(NULL)
      }
    )
    
    mock_error_record <- list(statements = mock_statements)
    class(mock_error_record) <- "pharmpy.model.external.nonmem.records.code_record.CodeRecord"
    
    mock_control_stream <- list(
      get_error_record = function() mock_error_record
    )
    
    model <- list(
      internals = list(
        control_stream = mock_control_stream
      )
    )
    
    result <- is_ltbs_model(model)
    expect_true(result, info = paste("Failed for expression:", expr))
  }
})
