test_that("get_estimation_options returns correct defaults and handles user options", {
  # Test NONMEM FOCE defaults
  defaults_nonmem_foce <- get_estimation_options("nonmem", "foce", NULL)
  expect_equal(
    defaults_nonmem_foce,
    list(
      MAXEVAL = "2000",
      PRINT = "5",
      POSTHOC = "",
      NOABORT = ""
    )
  )
  
  # Test NONMEM SAEM defaults
  defaults_nonmem_saem <- get_estimation_options("nonmem", "saem", NULL)
  expect_equal(
    defaults_nonmem_saem,
    list(
      NBURN = "500",
      NITER = "1000",
      ISAMPLE = "2"
    )
  )
  
  # Test user-specified options override defaults
  user_opts <- list(MAXEVAL = 1000, PRINT = 1)
  custom_opts <- get_estimation_options("nonmem", "foce", user_opts)
  expect_equal(
    custom_opts,
    list(
      MAXEVAL = "1000",
      PRINT = "1",
      POSTHOC = "",
      NOABORT = ""
    )
  )
  
  # Test nlmixr returns empty list (as per current implementation)
  nlmixr_opts <- get_estimation_options("nlmixr", "foce", NULL)
  expect_equal(nlmixr_opts, list())
}) 