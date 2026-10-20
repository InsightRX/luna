fake_results <- function(model_file, ofv) {
  tibble::tibble(
    model_file = model_file,
    ofv = ofv,
    minimization_successful = TRUE
  )
}

test_that("runs_as_table aborts without luna cache", {
  local_no_luna_cache()
  expect_error(runs_as_table(list()), "Luna cache not found")
})

test_that("runs_as_table builds table with dofv vs reference", {
  withr::local_options(width = 200)
  dir <- local_luna_project(runs = list(
    list(id = "run1", description = "base", tag = c("a", "b"), notes = "n1",
         tools = list(list(tool = "nonmem"), list(tool = "psn"), list(tool = "nonmem"))),
    list(id = "run2", description = "two-cmt", reference = "run1"),
    list(id = "run3", reference = "run9")
  ))
  writeLines("lst", file.path(dir, "run1.lst"))
  project <- .luna_cache$get("project")
  files <- file.path(dir, c("run1.mod", "run2.mod", "run3.mod"))
  mockery::stub(runs_as_table, "get_all_results", fake_results(files[1:2], c(100, 90)))
  out <- runs_as_table(project)
  expect_s3_class(out, "luna.run_table")
  expect_equal(out$id, c("run1", "run2", "run3"))
  expect_equal(out$reference, c("", "run1", "run9"))
  expect_equal(out$tag[1], "a, b")
  expect_equal(out$notes[1], "n1")
  expect_equal(out$tools[1], "nonmem,psn")
  expect_equal(out$status, c("finished", "not run", "not run"))
  expect_equal(out$ofv, c(100, 90, NA))
  expect_equal(out$dofv, c(NA, -10, NA))
  expect_false("minimization_successful" %in% names(out))
  expect_false(any(c("model_file", "output_file") %in% names(out)))
})

test_that("runs_as_table with raw = TRUE keeps all result columns", {
  dir <- local_luna_project()
  project <- .luna_cache$get("project")
  files <- file.path(dir, c("run1.mod", "run2.mod"))
  mockery::stub(runs_as_table, "get_all_results", fake_results(files, c(100, 95)))
  out <- runs_as_table(project, raw = TRUE)
  expect_false(inherits(out, "luna.run_table"))
  expect_true("minimization_successful" %in% names(out))
  expect_equal(out$dofv, c(NA, -5))
})

test_that("runs_as_table handles no available results", {
  withr::local_options(width = 200)
  local_luna_project()
  project <- .luna_cache$get("project")
  mockery::stub(runs_as_table, "get_all_results", tibble::tibble())
  out <- runs_as_table(project)
  expect_equal(out$ofv, c(NA_real_, NA_real_))
  expect_equal(out$dofv, c(NA_real_, NA_real_))
})

test_that("runs_as_table finds model in <id>/run.mod subfolder", {
  withr::local_options(width = 200)
  dir <- local_luna_project(runs = list(list(id = "run5", description = "sub")))
  dir.create(file.path(dir, "run5"))
  write_nm_model(file.path(dir, "run5", "run.mod"))
  project <- .luna_cache$get("project")
  mf <- NULL
  mockery::stub(runs_as_table, "get_all_results", function(x) {
    mf <<- x
    tibble::tibble()
  })
  runs_as_table(project)
  expect_equal(mf, file.path(dir, "run5", "run.mod"))
})

test_that("get_tools collapses unique tools", {
  expect_equal(get_tools(list(tools = list(list(tool = "a"), list(tool = "b"), list(tool = "a")))), "a,b")
  expect_equal(get_tools(list()), "")
})

test_that("get_all_results reads NONMEM results via pharmr", {
  calls <- list()
  mockery::stub(get_all_results, "pharmr::read_modelfit_results", function(esttool, path) {
    calls[[length(calls) + 1]] <<- list(esttool = esttool, path = path)
    if (path == "bad.mod") stop("parse error")
    list(
      ofv = list(123.4567),
      minimization_successful = TRUE,
      covstep_successful = FALSE,
      runtime_total = 12,
      warnings = list("w1"),
      significant_digits = 3.2,
      function_evaluations = 100
    )
  })
  out <- suppressMessages(get_all_results(c("run1.mod", "", NA, "bad.mod")))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$model_file, "run1.mod")
  expect_equal(out$ofv, 123.46)
  expect_true(out$minimization_successful)
  expect_false(out$covstep_successful)
  expect_equal(out$warnings, "w1")
  expect_equal(out$function_evaluations, 100)
  expect_equal(length(calls), 2)
  expect_equal(calls[[1]], list(esttool = "nonmem", path = "run1.mod"))
})

test_that("get_all_results returns empty tibble when nothing to parse", {
  out <- suppressMessages(get_all_results(c("", NA)))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})
