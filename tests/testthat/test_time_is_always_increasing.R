test_that("time_is_always_increasing correctly identifies increasing and non-increasing time sequences", {

  # Test case 1: Single subject with increasing time
  data1 <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 3)
  )
  expect_true(time_is_always_increasing(data1))
  
  # Test case 2: Single subject with non-increasing time
  data2 <- data.frame(
    ID = 1,
    TIME = c(0, 2, 1, 3)
  )
  expect_false(time_is_always_increasing(data2))
  
  # Test case 3: Multiple subjects with increasing time
  data3 <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1)
  )
  expect_true(time_is_always_increasing(data3))
  
  # Test case 4: Multiple subjects, one with non-increasing time
  data4 <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 1, 0)
  )
  expect_false(time_is_always_increasing(data4))
  
  # Test case 5: Equal times should be considered increasing (>= 0)
  data5 <- data.frame(
    ID = 1,
    TIME = c(0, 1, 1, 2)
  )
  expect_true(time_is_always_increasing(data5))

})
