tool_runs <- function() {
  list(
    list(
      id = "run1", description = "base",
      tools = list(
        list(tool = "vpc", options = list(list(samples = 100))),
        list(tool = "psn::bootstrap", options = list(list(samples = 50))),
        list(tool = "foo::bar")
      )
    )
  )
}

test_that("luna_tool aborts as job without tool", {
  local_luna_project(runs = tool_runs())
  expect_error(luna_tool("run1", as_job = TRUE), "specify which `tool`")
})

test_that("luna_tool starts a job when as_job is TRUE", {
  local_luna_project(runs = tool_runs())
  job_title <- NULL
  # the job's code block is not forced, so the (unexported) job body never runs
  mockery::stub(luna_tool, "job::job", function(title, ...) {
    job_title <<- title
    "job123"
  })
  expect_message(res <- luna_tool("run1", tool = "vpc", as_job = TRUE), "job123")
  expect_equal(res, "job123")
  expect_equal(job_title, "run1-vpc")
})

test_that("luna_tool lists specified tools when no tool given", {
  local_luna_project(runs = tool_runs())
  expect_message(res <- luna_tool("run1"), "vpc")
  expect_null(res)
})

test_that("luna_tool aborts for tool not defined for run", {
  local_luna_project(runs = tool_runs())
  expect_error(suppressMessages(luna_tool("run1", tool = "scm")), "No `scm` run defined")
})

test_that("luna_tool aborts for unsupported tool method", {
  local_luna_project(runs = tool_runs())
  mockery::stub(luna_tool, "pharmr::read_model", "model_obj")
  expect_error(
    suppressMessages(luna_tool("run1", tool = "foo::bar")),
    "foo not currently supported"
  )
})

test_that("luna_tool calls pharmpy tool with options", {
  folder <- local_luna_project(runs = tool_runs())
  mock_tool <- mockery::mock("vpc_result")
  mock_prepare <- mockery::mock(list(model = "prepared"))
  mockery::stub(luna_tool, "pharmr::read_model", "model_obj")
  mockery::stub(luna_tool, "prepare_run_folder", mock_prepare)
  mockery::stub(luna_tool, "pharmr::read_modelfit_results", "results")
  mockery::stub(luna_tool, "pharmr.extra::call_pharmpy_tool", mock_tool)
  res <- suppressMessages(luna_tool("run1", tool = "vpc"))
  expect_equal(res, "vpc_result")
  args <- mockery::mock_args(mock_tool)[[1]]
  expect_equal(args$model, "prepared")
  expect_equal(args$results, "results")
  expect_equal(args$tool, "vpc")
  expect_equal(args$options, list(samples = 100))
  prepare_args <- mockery::mock_args(mock_prepare)[[1]]
  expect_equal(prepare_args$id, "run1")
  expect_equal(prepare_args$model, "model_obj")
  expect_equal(prepare_args$path, normalizePath(folder))
})

test_that("luna_tool leaves an existing run folder untouched", {
  folder <- local_luna_project(runs = tool_runs())
  dir.create(file.path(folder, "run1"))
  mock_tool <- mockery::mock("vpc_result")
  mock_prepare <- mockery::mock()
  mockery::stub(luna_tool, "pharmr::read_model", "model_obj")
  mockery::stub(luna_tool, "prepare_run_folder", mock_prepare)
  mockery::stub(luna_tool, "pharmr::read_modelfit_results", "results")
  mockery::stub(luna_tool, "pharmr.extra::call_pharmpy_tool", mock_tool)
  suppressMessages(luna_tool("run1", tool = "vpc"))
  mockery::expect_called(mock_prepare, 0)
  expect_equal(mockery::mock_args(mock_tool)[[1]]$model, "model_obj")
})

test_that("luna_tool calls PsN tool with parsed args", {
  folder <- local_luna_project(runs = tool_runs())
  mock_psn <- mockery::mock("psn_result")
  mockery::stub(luna_tool, "pharmr::read_model", "model_obj")
  mockery::stub(luna_tool, "prepare_run_folder", function(...) list(model = "prepared"))
  mockery::stub(luna_tool, "pharmr.extra::parse_psn_args", "-samples=50")
  mockery::stub(luna_tool, "pharmr.extra::call_psn", mock_psn)
  expect_message(res <- luna_tool("run1", tool = "psn::bootstrap"), "Running")
  expect_equal(res, "psn_result")
  args <- mockery::mock_args(mock_psn)[[1]]
  expect_equal(args$tool, "bootstrap")
  expect_equal(args$options, "-samples=50")
  expect_equal(args$path, file.path(folder, "run1"))
})
