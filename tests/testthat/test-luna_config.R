test_that("get_luna_config aborts when config file is missing", {
  local_luna_home(config = FALSE)
  local_no_luna_cache()
  expect_error(get_luna_config(verbose = FALSE), "config file not found")
})

test_that("get_luna_config reads global config without project", {
  local_luna_home(config = list(general = list(a = 1)))
  local_no_luna_cache()
  expect_message(conf <- get_luna_config(), "cache not found")
  expect_equal(conf, list(general = list(a = 1)))
  expect_silent(get_luna_config(verbose = FALSE))
})

test_that("get_luna_config overrides global settings with project config", {
  local_luna_project(config = list(general = list(a = 1), other = list(b = 2)))
  project <- .luna_cache$get("project")
  project$yaml$project$config <- list(
    general = list(a = 5), other = list(b = 2), extra = list(c = 3)
  )
  .luna_cache$set("project", project)
  expect_message(conf <- get_luna_config(), "Overriding")
  expect_equal(conf$general$a, 5)
  expect_equal(conf$other$b, 2)
  expect_equal(conf$extra$c, 3)
})

test_that("luna_config opens config file in editor", {
  home <- local_luna_home()
  opened <- NULL
  mockery::stub(luna_config, "file.edit", function(f) opened <<- f)
  expect_message(luna_config(), "Opening file")
  expect_equal(normalizePath(opened), normalizePath(file.path(home, ".config", "luna", "config.yaml")))
})

test_that("luna_config aborts when config file is missing", {
  local_luna_home(config = FALSE)
  mockery::stub(luna_config, "file.edit", function(...) invisible(NULL))
  expect_error(luna_config(verbose = FALSE), "Cannot find file")
})

test_that("check_default_config_file creates config only when absent", {
  home <- local_luna_home(config = FALSE)
  unlink(file.path(home, ".config"), recursive = TRUE)
  conf_file <- file.path(home, ".config", "luna", "config.yaml")
  check_default_config_file()
  expect_true(file.exists(conf_file))
  expect_equal(
    readLines(conf_file),
    readLines(system.file("config", "default.yaml", package = "luna"))
  )
  writeLines("custom: 1", conf_file)
  check_default_config_file()
  expect_equal(readLines(conf_file), "custom: 1")
})

test_that("check_llm_key messages only when key is missing", {
  withr::local_envvar(OPENAI_API_KEY = "")
  expect_message(check_llm_key(), "OPENAI_API_KEY")
  withr::local_envvar(OPENAI_API_KEY = "abc")
  expect_silent(check_llm_key())
})

test_that(".onLoad sets up config file", {
  home <- local_luna_home(config = FALSE)
  unlink(file.path(home, ".config"), recursive = TRUE)
  withr::local_envvar(OPENAI_API_KEY = "abc")
  expect_null(.onLoad("lib", "luna"))
  expect_true(file.exists(file.path(home, ".config", "luna", "config.yaml")))
})
