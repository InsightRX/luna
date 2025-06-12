test_that("stack_encounters handles single encounters correctly", {
  # Create simple dataset with one encounter
  data <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    DV = c(1, 2, 3),
    EVID = c(0, 0, 0),
    AMT = c(0, 0, 0),
    MDV = c(0, 0, 0)
  )

  result <- stack_encounters(data)

  # Should return unchanged data when time is always increasing, but have
  # column ENC_TIME added
  expect_true("ENC_TIME" %in% names(result))
  expect_equal(result |> dplyr::select(-ENC_TIME), data)
})

test_that("stack_encounters correctly stacks multiple encounters", {
  # Create dataset with two encounters
  data <- data.frame(
    ID = c(1, 1, 1, 1, 1, 1),
    TIME = c(0, 1, 2, 0, 1, 2),
    DV = c(1, 2, 3, 4, 5, 6),
    EVID = c(0, 0, 0, 0, 0, 0),
    AMT = c(0, 0, 0, 0, 0, 0),
    MDV = c(0, 0, 0, 0, 0, 0)
  )

  result <- stack_encounters(data, gap = 10)

  # Check that TIME has been adjusted for second encounter
  expect_equal(result$TIME[1:3], c(0, 1, 2))
  expect_equal(result$TIME[4:7], c(10, 10, 11, 12))

  # Check that original times are preserved in ENC_TIME
  expect_equal(result$ENC_TIME, c(0, 1, 2, 0, 0, 1, 2))

  # Check that all original columns are preserved
  expect_true(all(names(data) %in% names(result)))
})

test_that("reset_encounters adds EVID=3 events correctly", {
  data <- data.frame(
    ID = c(1, 1, 1, 1, 1, 1),
    TIME = c(0, 1, 2, 0, 1, 2),
    DV = c(1, 2, 3, 4, 5, 6),
    EVID = c(0, 0, 0, 0, 0, 0),
    AMT = c(0, 0, 0, 0, 0, 0),
    MDV = c(0, 0, 0, 0, 0, 0)
  )

  # Test with reset_encounters = TRUE
  result_reset <- stack_encounters(data, gap = 10, reset_encounters = TRUE)

  # Should have one additional row for EVID=3 event
  expect_equal(nrow(result_reset), nrow(data) + 1)

  # Check EVID=3 event properties
  evid3_row <- result_reset[result_reset$EVID == 3, ]
  expect_equal(nrow(evid3_row), 1)
  expect_equal(evid3_row$TIME, 10)
  expect_equal(evid3_row$MDV, 1)
  expect_equal(evid3_row$DV, 0)
  expect_equal(evid3_row$AMT, 0)

  # Test with reset_encounters = FALSE
  result_no_reset <- stack_encounters(data, gap = 10, reset_encounters = FALSE)
  expect_equal(nrow(result_no_reset), nrow(data))
  expect_equal(sum(result_no_reset$EVID == 3), 0)
})

test_that("stack_encounters handles different gap values", {
  data <- data.frame(
    ID = c(1, 1, 1, 1, 1, 1),
    TIME = c(0, 1, 2, 0, 1, 2),
    DV = c(1, 2, 3, 4, 5, 6),
    EVID = c(0, 0, 0, 0, 0, 0),
    AMT = c(0, 0, 0, 0, 0, 0),
    MDV = c(0, 0, 0, 0, 0, 0)
  )

  result_gap_5 <- stack_encounters(data, gap = 5)
  result_gap_20 <- stack_encounters(data, gap = 20)

  # Check that second encounter starts at different times based on gap
  expect_equal(min(result_gap_5$TIME[4:6]), 5)
  expect_equal(min(result_gap_20$TIME[4:6]), 20)
})

test_that("stack_encounters handles multiple subjects correctly", {
  data <- data.frame(
    ID = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
    DV = 1:12,
    EVID = rep(0, 12),
    AMT = rep(0, 12),
    MDV = rep(0, 12)
  )

  result <- stack_encounters(data, gap = 10)

  # Check that stacking is done independently for each ID
  expect_equal(result$TIME[result$ID == 1][4:7], c(10, 10, 11, 12))
  expect_equal(result$TIME[result$ID == 2][4:7], c(10, 10, 11, 12))

  # Check that original order of IDs is preserved
  expect_equal(result$ID, c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2))  # Additional rows for EVID=3 events
})

