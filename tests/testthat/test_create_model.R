test_that("create_model call without arguments works", {
  mod <- create_model()
  expect_s3_class(mod, "pharmpy.model.external.nonmem.model.Model")
})

test_that("create_model basic functionality works", {
  # Create minimal test dataset
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70 
  )

  # Test basic oral model creation
  mod_oral <- create_model(
    route = "oral",
    data = test_data,
    verbose = FALSE
  )
  expect_s3_class(mod_oral, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("POP_KA", mod_oral$code))
  expect_true(grepl("TVKA", mod_oral$code))

  # Test basic IV model creation
  mod_iv <- create_model(
    route = "iv",
    data = test_data,
    verbose = FALSE
  )
  expect_s3_class(mod_oral, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("POP_CL", mod_iv$code))
  expect_true(grepl("TVCL", mod_iv$code))
  expect_true(!grepl("POP_KA", mod_iv$code))
  expect_true(!grepl("TVKA", mod_iv$code))
})

test_that("model features are correctly added", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test lag time
  mod_lag <- create_model(
    route = "oral",
    lag_time = TRUE,
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("ALAG", mod_lag$code))

  # Test transit compartments
  mod_transit <- create_model(
    route = "oral",
    n_transit_compartments = 3,
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("MDT", mod_transit$code))
  expect_true(grepl("\\$MODEL COMPARTMENT=\\(TRANSIT1 DEFDOSE\\)", mod_transit$code))

  # Test multiple compartments
  mod_multi2 <- create_model(
    route = "iv",
    n_cmt = 2,
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("QP1", mod_multi2$code))
  expect_true(grepl("VP1", mod_multi2$code))

  mod_multi3 <- create_model(
    route = "iv",
    n_cmt = 3,
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("QP1", mod_multi3$code))
  expect_true(grepl("VP1", mod_multi3$code))
  expect_true(grepl("QP2", mod_multi3$code))
  expect_true(grepl("VP2", mod_multi3$code))
})

test_that("estimation methods are correctly set", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test FOCE method
  mod_foce <- create_model(
    route = "iv",
    estimation_method = "foce",
    data = test_data,
    verbose = FALSE
  )
  steps <- mod_foce$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(steps$method))

  # Test SAEM method
  mod_saem <- create_model(
    route = "iv",
    estimation_method = "saem",
    data = test_data,
    verbose = FALSE
  )
  steps <- mod_saem$execution_steps$to_dataframe()
  expect_true("saem" %in% tolower(steps$method))
})

test_that("error handling works correctly", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test invalid route
  expect_error(
    create_model(route = "invalid"),
    "'arg' should be one of"
  )

  # Test invalid elimination
  expect_error(
    create_model(elimination = "invalid"),
    "'arg' should be one of"
  )

  # Test invalid tool
  expect_error(
    create_model(tool = "invalid"),
    "'arg' should be one of"
  )
})

test_that("IIV settings work as expected", {
  # Test default IIV settings
  mod <- create_model()
  expect_true("ETA_CL" %in% mod$random_variables$names)
  expect_true("ETA_V" %in% mod$random_variables$names)
  
  # Test custom IIV magnitudes
  mod <- create_model(iiv = list(CL = 0.4, V = 0.5))
  par_df <- mod$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.16) # 0.4^2
  expect_equal(par_df[pars == "IIV_V",]$value, 0.25)  # 0.5^2
  
  # Test different IIV types
  mod <- create_model(
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = list(CL = "add", V = "prop")
  )
  expect_match(
    as.character(mod$statements$find_assignment("CL")$expression),
    ".*ETA_CL \\+ .*",
    all = FALSE
  )
  expect_match(
    as.character(mod$statements$find_assignment("V")$expression),
    ".*TVV\\*\\(ETA_V \\+ 1\\).*",
    all = FALSE
  )
  
  # Test no IIV
  mod <- create_model(iiv = NULL)
  expect_false("IIV_CL" %in% mod$random_variables$names)
  expect_false("IIV_V" %in% mod$random_variables$names)
})

test_that("RUV settings work as expected", {
  # Test proportional error
  mod <- create_model(ruv = "proportional")
  expect_equal(
    "EPS_1*IPREDADJ + IPRED",
    as.character(mod$statements$find_assignment("Y")$expression)
  )
  
  # Test additive error
  mod <- create_model(ruv = "additive")
  expect_equal(
    "EPS_1*W + IPRED",
    as.character(mod$statements$find_assignment("Y")$expression)
  )
  
  # Test combined error
  mod <- create_model(ruv = "combined")
  expect_equal(
    "EPS_1*IPRED + EPS_2 + IPRED",
    as.character(mod$statements$find_assignment("Y")$expression)
  )
  
  # Test log-transformed both sides
  mod <- create_model(ruv = "ltbs")
  expect_equal(
    "EPS_1 + log(IPREDADJ)",
    as.character(mod$statements$find_assignment("Y")$expression)
  )
})

test_that("can create mu-referenced model", {
  mod <- create_model(mu_reference = TRUE)
  expect_s3_class(mod, "pharmpy.model.external.nonmem.model.Model")
})
