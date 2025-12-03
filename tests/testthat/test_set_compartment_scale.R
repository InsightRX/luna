# Tests for get_compartment_scale() ----

test_that("get_compartment_scale returns correct scale when S assignment exists with division", {
  # Mock assignment with division
  mock_assignment <- list(
    expression = "V2/1000"
  )
  class(mock_assignment) <- "pharmpy.model.statements.Assignment"

  mock_statements <- list(
    find_assignment = function(var) {
      if (var == "S2") return(mock_assignment)
      return(NULL)
    }
  )

  model <- list(statements = mock_statements)

  result <- get_compartment_scale(model, compartment = 2)
  expect_equal(result$variable, "V2")
  expect_equal(result$scale, 1000)
})

test_that("get_compartment_scale returns scale of 1 when no scale present", {
  # Mock assignment without division
  mock_assignment <- list(
    expression = "V1"
  )
  class(mock_assignment) <- "pharmpy.model.statements.Assignment"

  mock_statements <- list(
    find_assignment = function(var) {
      if (var == "S1") return(mock_assignment)
      return(NULL)
    }
  )

  model <- list(statements = mock_statements)

  result <- get_compartment_scale(model, compartment = 1)
  expect_equal(result$variable, "V1")
  expect_equal(result$scale, 1)
})

test_that("get_compartment_scale returns NULL when no S assignment found", {
  mock_statements <- list(
    find_assignment = function(var) NULL
  )

  model <- list(statements = mock_statements)

  result <- get_compartment_scale(model, compartment = 2)
  expect_null(result)
})

test_that("get_compartment_scale returns NULL when assignment is not Assignment class", {
  mock_statements <- list(
    find_assignment = function(var) "not_an_assignment"
  )

  model <- list(statements = mock_statements)

  result <- get_compartment_scale(model, compartment = 2)
  expect_null(result)
})

# Tests for scale_initial_estimates_pk() ----

test_that("scale_initial_estimates_pk scales basic PK parameters correctly", {
  # Mock parameters dataframe
  mock_params_df <- data.frame(
    value = c(10, 50, 2),
    row.names = c("CL", "V1", "KA")
  )

  mock_parameters <- list(
    to_dataframe = function() mock_params_df
  )

  model <- list(parameters = mock_parameters)

  # Mock pharmr::set_initial_estimates to capture what gets passed
  expected_inits <- list(CL = 100, V1 = 500) # scaled by 10
  actual_inits <- NULL

  mockery::stub(scale_initial_estimates_pk, "pharmr::set_initial_estimates",
    function(model, inits) {
      actual_inits <<- inits
      return(model)
    })

  result <- scale_initial_estimates_pk(model, scale = 10)

  expect_equal(actual_inits, expected_inits)
})

test_that("scale_initial_estimates_pk scales POP_ prefixed parameters", {
  mock_params_df <- data.frame(
    value = c(5, 25),
    row.names = c("POP_CL", "POP_V2")
  )

  mock_parameters <- list(
    to_dataframe = function() mock_params_df
  )

  model <- list(parameters = mock_parameters)

  expected_inits <- list(POP_CL = 10, POP_V2 = 50) # scaled by 2
  actual_inits <- NULL

  mockery::stub(scale_initial_estimates_pk, "pharmr::set_initial_estimates",
    function(model, inits) {
      actual_inits <<- inits
      return(model)
    })

  result <- scale_initial_estimates_pk(model, scale = 2)

  expect_equal(actual_inits, expected_inits)
})

test_that("scale_initial_estimates_pk ignores non-PK parameters", {
  mock_params_df <- data.frame(
    value = c(10, 0.1, 2),
    row.names = c("CL", "SIGMA", "OTHER_PARAM")
  )

  mock_parameters <- list(
    to_dataframe = function() mock_params_df
  )

  model <- list(parameters = mock_parameters)

  expected_inits <- list(CL = 50) # only CL should be scaled
  actual_inits <- NULL

  mockery::stub(scale_initial_estimates_pk, "pharmr::set_initial_estimates",
    function(model, inits) {
      actual_inits <<- inits
      return(model)
    })

  result <- scale_initial_estimates_pk(model, scale = 5)

  expect_equal(actual_inits, expected_inits)
})

test_that("scale_initial_estimates_pk handles all defined PK parameters", {
  all_pk_params <- c("CL", "V", "V1", "V2", "V3", "V4", "VP1", "VP2", "VP3",
                     "Q", "Q1", "Q2", "Q3", "QP1", "QP2", "QP3")

  mock_params_df <- data.frame(
    value = rep(10, length(all_pk_params)),
    row.names = all_pk_params
  )

  mock_parameters <- list(
    to_dataframe = function() mock_params_df
  )

  model <- list(parameters = mock_parameters)

  expected_inits <- as.list(rep(20, length(all_pk_params))) # scaled by 2
  names(expected_inits) <- all_pk_params
  actual_inits <- NULL

  mockery::stub(scale_initial_estimates_pk, "pharmr::set_initial_estimates",
    function(model, inits) {
      actual_inits <<- inits
      return(model)
    })

  result <- scale_initial_estimates_pk(model, scale = 2)

  expect_equal(actual_inits, expected_inits)
})

# Tests for set_compartment_scale() ----

test_that("set_compartment_scale infers compartment 2 for ADVAN 2, 4, 12", {
  for (i in c(1,2,3)) {
    model <- create_model(
      n_cmt = i,
      route = "oral"
    )
    expect_message(
      result <- set_compartment_scale(model, expression = list(variable = "V", scale = 1000)),
      "Scaling already specified"
    )
  }
})

test_that("set_compartment_scale infers compartment 1 for ADVAN 1, 3, 11", {
  for (i in c(1,2,3)) {
    model <- create_model(
      n_cmt = i,
      route = "iv"
    )
    expect_message(
      result <- set_compartment_scale(model, expression = list(variable = "V", scale = 1000)),
      "Scaling already specified"
    )
  }
})

test_that("set_compartment_scale adds new scaling when none exists", {
  model <- list(code = "other code\nmore code")

  mockery::stub(set_compartment_scale, "get_compartment_scale", NULL)
  mockery::stub(set_compartment_scale, "pharmr::read_model_from_string",
    function(code) {
      grepl("S2 = V/1000", code, fixed = TRUE)
      return(list(code = code))
    })
  mockery::stub(set_compartment_scale, "scale_initial_estimates_pk",
    function(model, scale) model)
  mockery::stub(set_compartment_scale, "find_pk_parameter",
    function(model, scale) "V")

  expect_message(
    result <- set_compartment_scale(
      model,
      compartment = 2,
      expression = list(variable = "V", scale = 1000)
    ),
    "No scaling specified for compartment 2, adding scale"
  )
})

test_that("set_compartment_scale updates existing scaling", {
  model <- list(code = "S2 = V2/500\nother code")

  mockery::stub(set_compartment_scale, "get_compartment_scale",
    list(variable = "V2", scale = "500"))
  mockery::stub(set_compartment_scale, "pharmr::read_model_from_string",
    function(code) {
      expect_true(grepl("S2 = V/1000", code, fixed = TRUE))
      return(list(code = code))
    })
  mockery::stub(set_compartment_scale, "scale_initial_estimates_pk",
    function(model, scale) model)
  mockery::stub(set_compartment_scale, "find_pk_parameter",
    function(model, scale) "V")

  expect_message(
    result <- set_compartment_scale(
      model,
      compartment = 2,
      expression = list(variable = "V", scale = 1000)
    ),
    "Scaling already specified for compartment 2, updating scale"
  )
})

test_that("set_compartment_scale calls scale_initial_estimates_pk when update_inits is TRUE", {
  model <- list(code = "S2 = V2")

  mockery::stub(set_compartment_scale, "get_compartment_scale", NULL)
  mockery::stub(set_compartment_scale, "pharmr::read_model_from_string",
    function(code) model)

  scale_called <- FALSE
  mockery::stub(set_compartment_scale, "find_pk_parameter",
                function(model, scale) "V")
  mockery::stub(set_compartment_scale, "scale_initial_estimates_pk",
    function(model, scale) {
      scale_called <<- TRUE
      expect_equal(scale, 1000)
      return(model)
    })

  result <- set_compartment_scale(
    model,
    compartment = 2,
    expression = list(variable = "V", scale = 1000),
    update_inits = TRUE
  )

  expect_true(scale_called)
})

test_that("set_compartment_scale skips scaling when update_inits is FALSE", {
  model <- list(code = "S2 = V2")

  mockery::stub(set_compartment_scale, "get_compartment_scale", NULL)
  mockery::stub(set_compartment_scale, "pharmr::read_model_from_string",
    function(code) model)

  scale_called <- FALSE
  mockery::stub(set_compartment_scale, "find_pk_parameter",
                function(model, scale) "V")
  mockery::stub(set_compartment_scale, "scale_initial_estimates_pk",
    function(model, scale) {
      scale_called <<- TRUE
      return(model)
    })

  result <- set_compartment_scale(
    model,
    compartment = 2,
    expression = list(variable = "V", scale = 1000),
    update_inits = FALSE
  )

  expect_false(scale_called)
})

## End-to-end tests for set_comparmtent_scale() with pharmpy model
test_that("set_compartment_scale works for pharmpy model", {
  model1 <- create_model(n_cmt = 1, route = "oral") # advan2
  model2 <- set_compartment_scale(
    model1,
    expression = list(variable = "V", scale = 1000)
  )
  expect_equal(
    model2$parameters$to_dataframe()$value[1],
    0.5 # KA, unchanged
  )
  expect_equal(
    model2$parameters$to_dataframe()$value[2],
    15000
  )
  expect_equal(
    model2$parameters$to_dataframe()$value[3],
    5000
  )
})

test_that("Finds parameter by common name (e.g. 'V' when actually named 'V2'", {
  model <- create_model(
    n_cmt = 2,
    route = "oral"
  )
  model2 <- model |>
    set_compartment_scale(
      compartment = 2,
      expression = list(
        variable = "V",
        scale = 1000
      )
    )
  expect_true(
    stringr::str_detect(model2$code, "S2 = V2/1000")
  )
})
