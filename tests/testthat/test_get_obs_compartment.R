test_that("get_obs_compartment() works for advan1", {
  advan1 <- create_model(route = "iv", n_cmt = 1)
  expect_equal(
    get_advan(advan1),
    1
  )
  expect_equal(
    get_obs_compartment(advan1),
    1
  )
  expect_equal(
    get_ode_size(advan1),
    0
  )
})

test_that("get_obs_compartment() works for advan2", {
  advan2 <- create_model(route = "oral", n_cmt = 1)
  expect_equal(
    get_advan(advan2),
    2
  )
  expect_equal(
    get_obs_compartment(advan2),
    2
  )
  expect_equal(
    get_ode_size(advan2),
    0
  )
})

test_that("get_obs_compartment() works for advan1", {
  advan3 <- create_model(route = "iv", n_cmt = 2)
  expect_equal(
    get_advan(advan3),
    3
  )
  expect_equal(
    get_obs_compartment(advan3),
    1
  )
  expect_equal(
    get_ode_size(advan3),
    0
  )
})

test_that("get_obs_compartment() works for advan4", {
  advan4 <- create_model(route = "oral", n_cmt = 2)
  expect_equal(
    get_advan(advan4),
    4
  )
  expect_equal(
    get_obs_compartment(advan4),
    2
  )
  expect_equal(
    get_ode_size(advan4),
    0
  )
})

test_that("get_obs_compartment() works for advan6", {
  advan6 <- create_model(
    route = "oral", n_cmt = 2,
    elimination = "michaelis-menten"
  )
  expect_true(
    get_advan(advan6) %in% c(6, 13)
  )
  expect_equal(
    get_obs_compartment(advan6),
    2
  )
  expect_equal(
    get_ode_size(advan6),
    3
  )
})

