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
