test_that("log_add aborts without luna cache", {
  local_no_luna_cache()
  expect_error(log_add("action", id = "run1"), "Luna cache not found")
  expect_error(luna_log(), "Luna cache not found")
})

test_that("log_add validates event type", {
  local_luna_project()
  expect_error(log_add("foo", id = "run1"))
})

test_that("log_add appends events and log_get_last_event returns latest", {
  local_luna_project()
  log_add("action", action = "modelfit", id = "run1")
  log_add("error", action = "modelfit", id = "run2", context = list(stdout = "oops"))
  log <- .luna_cache$get("log")
  expect_length(log, 2)
  last <- log_get_last_event()
  expect_equal(last$event, "error")
  expect_equal(last$id, "run2")
  expect_equal(last$context$stdout, "oops")
  expect_type(last$timestamp, "character")
})

test_that("log_add starts a new log when existing log is empty", {
  local_luna_project()
  .luna_cache$set("log", list())
  log_add("plot", action = "gof", id = "run1")
  expect_length(.luna_cache$get("log"), 1)
  expect_equal(log_get_last_event()$event, "plot")
})

test_that("luna_log returns data.frame of last n events", {
  local_luna_project()
  log_add("action", action = "modelfit", id = "run1")
  log_add("error", action = "modelfit", id = "run2")
  log_add("plot", action = "gof", id = "run2")
  out <- luna_log()
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_equal(names(out), c("event", "action", "id", "time"))
  expect_equal(out$event, c("action", "error", "plot"))
  expect_true(all(grepl("ago$", out$time)))
  out2 <- luna_log(n = 2)
  expect_equal(out2$event, c("error", "plot"))
})
