# tests/testthat/test-find_pk_parameter.R

test_that("find_pk_parameter returns parameter when it exists as-is in model", {
  # Mock the dependencies
  mock_model <- list(params = c("CL", "V", "KA"))

  # Mock get_pk_parameters to return the model parameters
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL", "V", "KA"))

  result <- find_pk_parameter("V", mock_model)
  expect_equal(result, "V")

  result <- find_pk_parameter("CL", mock_model)
  expect_equal(result, "CL")
})

test_that("find_pk_parameter maps parameters correctly for ADVAN 1, 3, 11", {
  mock_model <- list(advan = 1)

  # Mock dependencies
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL", "V1"))
  mockery::stub(find_pk_parameter, "get_advan", 1)

  # Test V -> V1 mapping for ADVAN 1
  expect_message(
    result <- find_pk_parameter("V", mock_model),
    "Found parameter V in model as V1"
  )
  expect_equal(result, "V1")

  # Test with ADVAN 3
  mockery::stub(find_pk_parameter, "get_advan", 3)
  expect_message(
    result <- find_pk_parameter("V", mock_model),
    "Found parameter V in model as V1"
  )
  expect_equal(result, "V1")

  # Test with ADVAN 11
  mockery::stub(find_pk_parameter, "get_advan", 11)
  expect_message(
    result <- find_pk_parameter("V", mock_model),
    "Found parameter V in model as V1"
  )
  expect_equal(result, "V1")
})

test_that("find_pk_parameter maps parameters correctly for other ADVAN numbers", {
  mock_model <- list(advan = 2)

  # Mock dependencies
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL", "V2"))
  mockery::stub(find_pk_parameter, "get_advan", 2)

  # Test V -> V2 mapping for ADVAN 2
  expect_message(
    result <- find_pk_parameter("V", mock_model),
    "Found parameter V in model as V2"
  )
  expect_equal(result, "V2")

  # Test with ADVAN 4
  mockery::stub(find_pk_parameter, "get_advan", 4)
  expect_message(
    result <- find_pk_parameter("V", mock_model),
    "Found parameter V in model as V2"
  )
  expect_equal(result, "V2")
})

test_that("find_pk_parameter maps all parameters correctly for ADVAN 1, 3, 11", {
  mock_model <- list()

  # Mock dependencies
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL"))
  mockery::stub(find_pk_parameter, "get_advan", 1)

  # Test all mappings for ADVAN 1/3/11
  expected_mappings <- list(
    "V" = "V1",
    "Q" = "QP1",
    "V2" = "VP1",
    "V3" = "VP2"
  )

  for (param in names(expected_mappings)) {
    expect_message(
      result <- find_pk_parameter(param, mock_model),
      paste("Found parameter", param, "in model as", expected_mappings[[param]])
    )
    expect_equal(result, expected_mappings[[param]])
  }
})

test_that("find_pk_parameter maps all parameters correctly for other ADVAN numbers", {
  mock_model <- list()

  # Mock dependencies
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL"))
  mockery::stub(find_pk_parameter, "get_advan", 2)

  # Test all mappings for other ADVANs
  expected_mappings <- list(
    "V" = "V2",
    "Q" = "QP1",
    "V3" = "VP1",
    "V4" = "VP2"
  )

  for (param in names(expected_mappings)) {
    expect_message(
      result <- find_pk_parameter(param, mock_model),
      paste("Found parameter", param, "in model as", expected_mappings[[param]])
    )
    expect_equal(result, expected_mappings[[param]])
  }
})

test_that("find_pk_parameter warns and returns original parameter when not found in mapping", {
  mock_model <- list()

  # Mock dependencies
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL"))
  mockery::stub(find_pk_parameter, "get_advan", 1)

  # Test parameter not in mapping
  expect_warning(
    result <- find_pk_parameter("UNKNOWN_PARAM", mock_model),
    "Could not find parameter UNKNOWN_PARAM in model as UNKNOWN_PARAM, nor under different name."
  )
  expect_equal(result, "UNKNOWN_PARAM")
})

test_that("find_pk_parameter handles edge cases", {
  mock_model <- list()

  # Test empty parameter name
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL", "V"))

  expect_error(
    result <- find_pk_parameter("", mock_model)
  )

  # Test NULL parameter
  expect_error(find_pk_parameter(NULL, mock_model))
})

test_that("find_pk_parameter doesn't use mapping when parameter exists as-is", {
  mock_model <- list()

  # Mock V exists in model parameters, so mapping shouldn't be used
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL", "V", "KA"))

  # Even though ADVAN would map V to V1, since V exists, it should return V
  result <- find_pk_parameter("V", mock_model)
  expect_equal(result, "V")

  # Verify get_advan is not called when parameter exists as-is
  # (This is harder to test directly, but the function should return early)
})

test_that("find_pk_parameter integration test with realistic parameter names", {
  mock_model <- list()

  # Test realistic scenario: model has V1, user asks for V
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL", "V1", "KA"))
  mockery::stub(find_pk_parameter, "get_advan", 1)

  expect_message(
    result <- find_pk_parameter("V", mock_model),
    "Found parameter V in model as V1"
  )
  expect_equal(result, "V1")

  # Test realistic scenario: model has V2, user asks for V
  mockery::stub(find_pk_parameter, "pharmr::get_pk_parameters", c("CL", "V2", "Q"))
  mockery::stub(find_pk_parameter, "get_advan", 2)

  expect_message(
    result <- find_pk_parameter("V", mock_model),
    "Found parameter V in model as V2"
  )
  expect_equal(result, "V2")
})
