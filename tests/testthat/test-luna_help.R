fake_chat <- function(env) {
  function(system_prompt, model) {
    env$model <- model
    env$system_prompt <- system_prompt
    list(chat = function(...) {
      env$args <- list(...)
      "LLM answer"
    })
  }
}

test_that("luna_help aborts without luna cache", {
  local_no_luna_cache()
  expect_error(luna_help("q"), "Luna cache not found")
})

test_that("luna_help sends user prompt to LLM", {
  local_luna_project()
  env <- new.env()
  mockery::stub(luna_help, "ellmer::chat_openai", fake_chat(env))
  msgs <- testthat::capture_messages(luna_help("How do I?", model = "my-model"))
  expect_true(any(grepl("LLM answer", msgs)))
  expect_equal(env$model, "my-model")
  expect_match(env$system_prompt, "NONMEM")
  expect_equal(env$args[[1]], "How do I?")
  expect_false(env$args$echo)
})

test_that("luna_help asks about last error event", {
  local_luna_project()
  log_add("error", id = "run1", context = list(stdout = "MY NONMEM ERROR"))
  env <- new.env()
  mockery::stub(luna_help, "ellmer::chat_openai", fake_chat(env))
  msgs <- testthat::capture_messages(luna_help())
  expect_true(any(grepl("Getting help on error", msgs)))
  expect_match(env$args[[1]], "MY NONMEM ERROR")
})

test_that("luna_help describes last plot event", {
  local_luna_project()
  log_add("plot", action = "gof", id = "run1")
  env <- new.env()
  mockery::stub(luna_help, "ellmer::chat_openai", fake_chat(env))
  mockery::stub(luna_help, "ellmer::content_image_plot", function() "PLOT")
  suppressMessages(luna_help(verbose = FALSE))
  expect_match(env$args[[1]], "Describe this plot")
  expect_equal(env$args[[3]], "PLOT")
})

test_that("luna_help aborts for other event types", {
  local_luna_project()
  log_add("action", id = "run1")
  env <- new.env()
  mockery::stub(luna_help, "ellmer::chat_openai", fake_chat(env))
  expect_error(luna_help(verbose = FALSE), "anything I can help with")
})

test_that("luna_help aborts when there is no event", {
  local_luna_project()
  .luna_cache$set("log", list())
  expect_error(luna_help(verbose = TRUE), "No recent event found")
})
