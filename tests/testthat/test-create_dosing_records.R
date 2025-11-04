test_that("create_dosing_records creates correct basic dosing schedule", {
  # Setup test data
  test_data <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    EVID = c(1, 0, 0),
    CMT = c(1, 1, 1),
    AMT = c(100, 0, 0),
    MDV = c(1, 0, 0),
    DV = c(0, 5, 3)
  )
  
  regimen <- list(
    dose = c(500),
    interval = 12,
    time = c(0, 12, 24),
    route = "oral"
  )
  
  result <- create_dosing_records(
    regimen = regimen,
    data = test_data,
    n_subjects = 2,
    dictionary = NULL
  )
  
  # Test basic structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6) # 3 doses × 2 subjects
  expect_equal(unique(result$ID), c(1, 2))
  
  # Test dosing times
  expected_times <- c(0, 12, 24)
  expect_equal(unique(result$TIME), expected_times)
  
  # Test dose amounts
  expect_true(all(result$AMT == 500))
  expect_true(all(result$EVID == 1))
  expect_true(all(result$MDV == 1))
  expect_true(all(result$DV == 0))
})

test_that("create_dosing_records handles IV route with infusion time", {
  test_data <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0),
    CMT = c(2, 2),
    AMT = c(100, 0),
    MDV = c(1, 0),
    DV = c(0, 5)
  )
  
  regimen <- list(
    dose = 1000,
    interval = 24,
    time = c(0, 24),
    route = "iv",
    t_inf = 2
  )
  
  result <- create_dosing_records(
    regimen = regimen,
    data = test_data,
    n_subjects = 1,
    dictionary = NULL
  )
  
  # Test that RATE is calculated correctly
  expect_true("RATE" %in% names(result))
  expect_equal(unique(result$RATE), 500) # 1000/2
  expect_equal(unique(result$CMT), 2) # Should use CMT from template
})

test_that("create_dosing_records handles SC route with infusion time", {
  test_data <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0),
    CMT = c(1, 1),
    AMT = c(100, 0),
    MDV = c(1, 0),
    DV = c(0, 5)
  )
  
  regimen <- list(
    dose = 500,
    interval = 12,
    time = c(0, 12),
    route = "sc",
    t_inf = 0.5
  )
  
  result <- create_dosing_records(
    regimen = regimen,
    data = test_data,
    n_subjects = 1,
    dictionary = NULL
  )
  
  expect_true("RATE" %in% names(result))
  expect_equal(unique(result$RATE), 1000) # 500/0.5
})

test_that("create_dosing_records handles vector doses correctly", {
  test_data <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0),
    CMT = c(1, 1),
    AMT = c(100, 0),
    MDV = c(1, 0),
    DV = c(0, 5)
  )
  
  regimen <- list(
    dose = c(500, 750), # Vector of doses - should use first element
    interval = 12,
    time = c(0, 12),
    route = "oral"
  )
  
  result <- create_dosing_records(
    regimen = regimen,
    data = test_data,
    n_subjects = 1,
    dictionary = NULL
  )
  
  expect_equal(unique(result$AMT), c(500, 750)) # Should use first dose
})

test_that("create_dosing_records fails with missing required arguments", {
  test_data <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0),
    CMT = c(1, 1)
  )
  
  # Missing 'dose'
  regimen_no_dose <- list(
    interval = 12,
    time = c(0, 12, 24),
    route = "oral"
  )
  
  expect_error(
    create_dosing_records(regimen_no_dose, test_data, 1, NULL)
  )
    
  # Missing 'route'
  regimen_no_route <- list(
    dose = 500,
    interval = 12,
    time = c(0, 12, 24)
  )
  
  expect_error(
    create_dosing_records(regimen_no_route, test_data, 1, NULL)
  )
})

test_that("create_dosing_records handles single dose correctly", {
  test_data <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0),
    CMT = c(1, 1),
    AMT = c(100, 0),
    MDV = c(1, 0),
    DV = c(0, 5)
  )
  
  regimen <- list(
    dose = 1000,
    interval = 24,
    time = 0,
    route = "iv"
  )
  
  result <- create_dosing_records(
    regimen = regimen,
    data = test_data,
    n_subjects = 2,
    dictionary = NULL
  )
  
  expect_equal(nrow(result), 2) # 1 dose × 2 subjects
  expect_equal(unique(result$TIME), 0)
  expect_equal(unique(result$AMT), 1000)
})

test_that("create_dosing_records handles multiple subjects correctly", {
  test_data <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0),
    CMT = c(1, 1),
    AMT = c(100, 0),
    MDV = c(1, 0),
    DV = c(0, 5)
  )
  
  regimen <- list(
    dose = 250,
    interval = 6,
    time = c(0, 6, 12, 18),
    route = "oral"
  )
  
  n_subjects <- 5
  result <- create_dosing_records(
    regimen = regimen,
    data = test_data,
    n_subjects = n_subjects,
    dictionary = NULL
  )
  
  expect_equal(length(unique(result$ID)), n_subjects)
  expect_equal(nrow(result), 4 * n_subjects) # 4 doses × 5 subjects
  
  # Check each subject has correct dosing times
  for(id in 1:n_subjects) {
    subject_data <- result[result$ID == id, ]
    expect_equal(subject_data$TIME, c(0, 6, 12, 18))
    expect_true(all(subject_data$AMT == 250))
  }
})
