list_config <- function(columns) {
  config <- yaml::read_yaml(system.file("config", "default.yaml", package = "luna"))
  config$general$runs$as_job <- FALSE
  config$general$list$columns <- columns
  config
}

## avoid calling pharmpy when parsing results
stub_results <- function(fun) {
  rat <- runs_as_table
  mockery::stub(rat, "get_all_results", tibble::tibble())
  mockery::stub(fun, "runs_as_table", rat)
  fun
}

test_that("luna_list aborts without luna cache", {
  local_no_luna_cache()
  expect_error(luna_list(), "Luna cache not found")
})

test_that("luna_list selects columns from config", {
  withr::local_options(width = 200)
  local_luna_project()
  out <- suppressMessages(stub_results(luna_list)())
  expect_s3_class(out, "luna.run_table")
  expect_equal(
    names(out),
    c("id", "description", "notes", "tools", "status", "ofv", "finished")
  )
  expect_equal(out$id, c("run1", "run2"))
  expect_equal(out$status, c("not run", "not run"))
})

test_that("luna_list reflects YAML changes on disk", {
  withr::local_options(width = 200)
  dir <- local_luna_project()
  yml_file <- file.path(dir, "testproj.yaml")
  yml <- yaml::read_yaml(yml_file)
  yml$runs[[1]]$description <- "updated description"
  yaml::write_yaml(yml, yml_file)
  out <- suppressMessages(stub_results(luna_list)())
  expect_equal(out$description[1], "updated description")
})

test_that("luna_list warns on unknown columns", {
  withr::local_options(width = 200)
  local_luna_project(config = list_config(list(id = TRUE, foo = TRUE)))
  expect_warning(
    out <- suppressMessages(stub_results(luna_list)()),
    "not available"
  )
  expect_equal(names(out), "id")
})

test_that("luna_list warns when no columns selected", {
  withr::local_options(width = 200)
  local_luna_project(config = list_config(list(id = FALSE, description = FALSE)))
  expect_warning(
    out <- suppressMessages(stub_results(luna_list)()),
    "No columns selected"
  )
  expect_true(all(c("id", "description", "status") %in% names(out)))
})

test_that("luna_list returns all columns when no column config", {
  withr::local_options(width = 200)
  config <- list_config(NULL)
  config$general$list <- NULL
  local_luna_project(config = config)
  out <- suppressMessages(stub_results(luna_list)())
  expect_true(all(c("id", "reference", "tag", "dofv") %in% names(out)))
})

test_that("luna_list with raw = TRUE returns all information", {
  local_luna_project()
  out <- suppressMessages(stub_results(luna_list)(raw = TRUE))
  expect_false(inherits(out, "luna.run_table"))
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("id", "reference", "tag", "notes", "dofv") %in% names(out)))
})

test_that("luna_list works on a project without runs", {
  local_luna_project(runs = list())
  expect_no_warning(out <- suppressMessages(luna_list()))
  expect_equal(nrow(out), 0)
})
