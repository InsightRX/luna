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

test_that("stack_encounter() doesn't error when DV is character", {
  dat <- structure(
    list(
      ID = c(10011001, 10011001, 10011001, 10011001, 10011001),
      DV = c("<0.05", NA, "<0.05", "<0.05", "<0.05"),
      TIME = c(0, 0, 0.5, 1, 1.5),
      EVID = c(0, 1, 0, 0, 0),
      AMT = c(NA, 100, NA, NA, NA),
      GROUP = c("A", "A", "A", "A", "A"),
      AGE = c(60.0027684797834, 60.0027684797834, 60.0027684797834, 60.0027684797834, 60.0027684797834),
      WEIGHT = c(54.1672683117584, 54.1672683117584, 54.1672683117584, 54.1672683117584, 54.1672683117584)
    ),
    row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame")
  )
  nm_dat <- stack_encounters(
    data = dat,
    verbose = TRUE
  )
  expect_equal(
    names(nm_dat),
    c("ID", "DV", "TIME", "EVID", "AMT", "GROUP", "AGE", "WEIGHT", "ENC_TIME")
  )
})
