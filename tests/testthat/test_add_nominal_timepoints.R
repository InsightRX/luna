test_that("Adding nominal timepoints works", {
  
  dat <- get_example_dataset("genentech", "test_mod_1")$test_mod_1
  expect_message(
    dat <- add_nominal_timepoints(
      dat, 
      "time",
      "NOM_TIME",
      adjust = 0.7,
      verbose = TRUE
    )
  )
  expect_equal(
    dat$NOM_TIME[1:20], 
    c(0, 0, 0, 0, 21, 21, 21, 21, 21, 21, 42, 42, 42, 42, 42, 42, 63, 63, 0, 0)
  )

})

