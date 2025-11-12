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

test_that("IIV argument works with multi-compartment models", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test 2-compartment model
  mod_2cmt <- create_model(
    route = "iv",
    n_cmt = 2,
    iiv = list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_V1" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_Q" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_V2" %in% mod_2cmt$random_variables$names)

  # Test 2-compartment model with correlation
  mod_2cmt <- create_model(
    route = "iv",
    n_cmt = 2,
    iiv = list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5, "CL~V1" = 0.4),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_V1" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_Q" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_V2" %in% mod_2cmt$random_variables$names)

  expect_true(grepl("\\$OMEGA BLOCK\\(2\\)", mod_2cmt$code))
  expect_true(grepl("IIV_Q", mod_2cmt$code))
  expect_true(grepl("IIV_V2", mod_2cmt$code))

  # Test 2-compartment model with multiple correlations
  mod_2cmt2 <- create_model(
    route = "iv",
    n_cmt = 2,
    tool = "nlmixr2",
    iiv = list(
      CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5,
      "CL~V1" = 0.4, "Q~V2" = 0.3
    ),
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("ETA_V1 \\+ ETA_Q \\+ ETA_V2 \\+ ETA_CL", mod_2cmt2$code))
  expect_true(grepl("0.09,", mod_2cmt2$code))
  expect_true(grepl("0.001, 0.16,", mod_2cmt2$code))
  expect_true(grepl("0.001, 0.06, 0.25", mod_2cmt2$code))
  expect_true(grepl("0.024, 0.001, 0.001, 0.04", mod_2cmt2$code))

  ## create_model works when `parameters` table is requested
  mod_2cmt3 <- create_model(
    route = "iv",
    n_cmt = 2,
    iiv = list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5, "CL~V2" = 0.4),
    data = test_data,
    tables = c("parameters"),
    verbose = FALSE
  )
  expect_true(grepl("\\$OMEGA BLOCK\\(2\\)", mod_2cmt3$code))
  expect_true(grepl("ID CL V1 Q V2", mod_2cmt3$code))
})

test_that("IIV argument handles edge cases correctly", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test with single parameter IIV
  mod_single <- create_model(
    route = "iv",
    iiv = list(CL = 0.2),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_single$random_variables$names)
  expect_false("ETA_V" %in% mod_single$random_variables$names)

  # Test with very small IIV values
  mod_small <- create_model(
    route = "iv",
    iiv = list(CL = 0.01, V = 0.02),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_small$random_variables$names)
  expect_true("ETA_V" %in% mod_small$random_variables$names)

  # Test with large IIV values
  mod_large <- create_model(
    route = "iv",
    iiv = list(CL = 1.0, V = 1.5),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_large$random_variables$names)
  expect_true("ETA_V" %in% mod_large$random_variables$names)
})

test_that("IIV argument works with bioavailability parameter", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test with bioavailability parameter and IIV
  mod_bio <- create_model(
    route = "oral",
    bioavailability = TRUE,
    iiv = list(CL = 0.2, V = 0.3, BIO = 0.4),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_bio$random_variables$names)
  expect_true("ETA_V" %in% mod_bio$random_variables$names)
  expect_true("ETA_BIO" %in% mod_bio$random_variables$names)
})

test_that("IIV argument works with Michaelis-Menten elimination", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test with Michaelis-Menten elimination and IIV
  mod_mm <- create_model(
    route = "iv",
    elimination = "michaelis-menten",
    iiv = list(CL = 0.2, V = 0.3, KM = 0.5),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_mm$random_variables$names)
  expect_true("ETA_V" %in% mod_mm$random_variables$names)
  expect_true("ETA_KM" %in% mod_mm$random_variables$names)
})

test_that("IIV argument preserves parameter initial estimates correctly", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test that IIV values are correctly converted from SD to variance
  mod <- create_model(
    route = "iv",
    iiv = list(CL = 0.3, V = 0.4),
    data = test_data,
    verbose = FALSE
  )

  par_df <- mod$parameters$to_dataframe()
  pars <- rownames(par_df)

  # Check that IIV parameters are set to variance (SD^2)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.09)  # 0.3^2
  expect_equal(par_df[pars == "IIV_V",]$value, 0.16)   # 0.4^2

  # Check that population parameters are preserved
  expect_true("POP_CL" %in% pars)
  expect_true("POP_V" %in% pars)
})

test_that("IIV covariance works", {
  model_pk <- create_model(
    route = "iv",
    n_cmt = 2,
    tool = "nonmem",
    estimation_method = "foce",
    elimination = "linear",
    iiv = list(CL = 0.2, V = 0.2),
    iiv_type = "exp",
    ruv = "additive",
    uncertainty_method = "none",
    name = "run1",
    tables = c("fit"),
    verbose = T
  )

  model_pk2 <- set_covariance(model_pk, list("CL~V1" = 0.32))
  par_df <- model_pk2$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_true(all(c("IIV_CL", "IIV_V1") %in% pars))
  expect_true(stringr::str_detect(model_pk2$code, "\\$OMEGA BLOCK\\(2\\)"))

  model_pk3 <- set_iiv(model_pk, list("CL" = 0.1, "V1" = 0.1, "QP1" = 0.1))
  par_df <- model_pk3$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_true(all(c("IIV_QP1", "IIV_CL", "IIV_V1") %in% pars))
  expect_false(stringr::str_detect(model_pk3$code, "\\$OMEGA BLOCK\\(2\\)"))

  model_pk4 <- set_covariance(model_pk3, list("QP1~V1" = 0.32))
  par_df <- model_pk4$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_true(all(c("IIV_QP1", "IIV_CL", "IIV_V1") %in% pars))
  expect_true(stringr::str_detect(model_pk2$code, "\\$OMEGA BLOCK\\(2\\)"))

})

test_that("IIV argument works with different tools", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test with nlmixr tool
  mod_nlmixr <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    tool = "nlmixr",
    data = test_data,
    verbose = FALSE
  )
  # nlmixr models should still have the same IIV structure
  expect_true("ETA_CL" %in% mod_nlmixr$random_variables$names)
  expect_true("ETA_V" %in% mod_nlmixr$random_variables$names)
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

test_that("LTBS model is handled, and LNDV is set to DV", {
  nm_data <- data.frame(
    ID = c(1, 1,1,1,1),
    AMT = c(100, 0,0,0,0),
    TIME = c(0, 1,2,3,4),
    DV = c(0, 1,2,3,4),
    LNDV = c(0, -2,-1,0,1),
    EVID = c(1, 0, 0, 0, 0),
    CMT = c(1, 1,1,1,1)
  )
  mod <- create_model(
    ruv = "ltbs",
    data = nm_data
  )
  expect_equal(
    mod$dataset$DV,
    mod$dataset$LNDV
  )
  expect_true(
    "ODV" %in% names(mod$dataset)
  )
})

test_that("can create mu-referenced model", {
  mod <- create_model(mu_reference = TRUE)
  expect_s3_class(mod, "pharmpy.model.external.nonmem.model.Model")
})

test_that("IIV argument handles all input formats correctly", {
  # Test data for consistent testing
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test 1: Character "all" - should add IIV to all parameters
  mod_all <- create_model(
    route = "iv",
    iiv = "all",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_all$random_variables$names)
  expect_true("ETA_V" %in% mod_all$random_variables$names)

  # Test 2: Character "basic" - should add IIV only to CL and V
  mod_basic <- create_model(
    route = "iv",
    iiv = "basic",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_basic$random_variables$names)
  expect_true("ETA_V" %in% mod_basic$random_variables$names)

  # Test 3: Character vector of parameter names
  mod_char_vec <- create_model(
    route = "iv",
    iiv = c("CL", "V"),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_char_vec$random_variables$names)
  expect_true("ETA_V" %in% mod_char_vec$random_variables$names)

  # Test 4: List with numeric values (SD scale)
  mod_list <- create_model(
    route = "iv",
    iiv = list(CL = 0.3, V = 0.4),
    data = test_data,
    verbose = FALSE
  )
  par_df <- mod_list$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.09)  # 0.3^2
  expect_equal(par_df[pars == "IIV_V",]$value, 0.16)   # 0.4^2

  # Test 5: NULL - should remove all IIV
  mod_null <- create_model(
    route = "iv",
    iiv = NULL,
    data = test_data,
    verbose = FALSE
  )
  ## There always has to remain one ETA (in current Pharmpy version)
  expect_true("ETA_CL" %in% mod_null$random_variables$names)
  expect_false("ETA_V" %in% mod_null$random_variables$names)
})

test_that("IIV argument works with different routes", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test IV route with IIV
  mod_iv <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_iv$random_variables$names)
  expect_true("ETA_V" %in% mod_iv$random_variables$names)

  # Test oral route with IIV (should include KA parameter)
  mod_oral <- create_model(
    route = "oral",
    iiv = list(CL = 0.2, V = 0.3, KA = 0.4),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_oral$random_variables$names)
  expect_true("ETA_V" %in% mod_oral$random_variables$names)
  expect_true("ETA_KA" %in% mod_oral$random_variables$names)
})

test_that("IIV argument works with multi-compartment models", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test 2-compartment model
  mod_2cmt <- create_model(
    route = "iv",
    n_cmt = 2,
    iiv = list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_V1" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_Q" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_V2" %in% mod_2cmt$random_variables$names)

  # Test 3-compartment model
  mod_3cmt <- create_model(
    route = "iv",
    n_cmt = 3,
    iiv = list(CL = 0.2, V1 = 0.3, Q2 = 0.4, V2 = 0.5, Q3 = 0.6, V3 = 0.7),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_V1" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_Q2" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_V2" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_Q3" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_V3" %in% mod_3cmt$random_variables$names)
})

test_that("IIV argument works with different IIV types", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test exponential IIV (default)
  mod_exp <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = "exp",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_exp$random_variables$names)
  expect_true("ETA_V" %in% mod_exp$random_variables$names)

  # Test additive IIV
  mod_add <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = "add",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_add$random_variables$names)
  expect_true("ETA_V" %in% mod_add$random_variables$names)

  # Test proportional IIV
  mod_prop <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = "prop",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_prop$random_variables$names)
  expect_true("ETA_V" %in% mod_prop$random_variables$names)

  # Test mixed IIV types
  mod_mixed <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = list(CL = "add", V = "exp"),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_mixed$random_variables$names)
  expect_true("ETA_V" %in% mod_mixed$random_variables$names)
})

test_that("create_model with scaling works", {
  # Create minimal test dataset
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 15, 9),  # mg/L
    AMT = c(1, 0, 0),  # 1 g
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test basic oral model creation, when no IIV on V
  mod_scale1 <- create_model(
    route = "oral",
    data = test_data,
    scale_observations = 1000,
    verbose = FALSE
  )
  expect_true(stringr::str_detect(mod_scale1$code, "S2 = V/1000"))
  expect_true(stringr::str_detect(mod_scale1$code, "\\$THETA  \\(0, 34.1\\)"))
  expect_true(stringr::str_detect(mod_scale1$code, "\\$THETA  \\(0, 66.7\\)"))

  # Test 1-cmt oral model creation, with IIV on V
  mod_scale2 <- create_model(
    route = "oral",
    data = test_data,
    n_cmt = 1,
    iiv = list(CL = .2, V = .3),
    scale_observations = 1000,
    verbose = FALSE
  )
  expect_true(stringr::str_detect(mod_scale2$code, "S2 = V/1000"))
  expect_true(stringr::str_detect(mod_scale2$code, "\\$THETA  \\(0, 34.1\\)"))
  expect_true(stringr::str_detect(mod_scale2$code, "\\$THETA  \\(0, 66.7\\)"))

  # Test 2-cmt oral model creation, with IIV on V
  mod_scale3 <- create_model(
    route = "oral",
    data = test_data,
    n_cmt = 2,
    iiv = list(CL = .2, V2 = .3),
    scale_observations = 1000,
    verbose = FALSE
  )
  expect_true(stringr::str_detect(mod_scale3$code, "S2 = V2/1000"))
  expect_true(stringr::str_detect(mod_scale3$code, "\\$THETA  \\(0, 34.1\\)"))
  expect_true(stringr::str_detect(mod_scale3$code, "\\$THETA  \\(0, 66.7\\)"))
  expect_true(stringr::str_detect(mod_scale3$code, "\\$THETA  \\(0,133.0\\)"))

})

test_that("create_model BLQ with LLOQ coded in DV works", {
  # Create minimal test dataset
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, "<3"),
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
  expect_equal(mod_oral$dataset$LLOQ, c(0, 0, 3))
})
