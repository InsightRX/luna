fake_pharmpy_model <- function(dataset = data.frame(ID = 1)) {
  structure(list(dataset = dataset), class = "pharmpy.model.model.Model")
}

run_config <- function(modelfit = list(method = "pharmpy", console = TRUE)) {
  # tools.nonmem.nmfe is set because ifelse0() errors when the fallback is NULL
  list(
    general = list(runs = list(as_job = FALSE)),
    tools = list(modelfit = modelfit, nonmem = list(nmfe = "/opt/nm/fallback"))
  )
}

test_that("luna_run aborts for multiple ids", {
  expect_error(luna_run(c("run1", "run2")), "multiple runs")
})

test_that("luna_run aborts when model file is missing", {
  local_luna_project()
  expect_error(suppressMessages(luna_run("run9")), "not found")
})

test_that("luna_run aborts for non-pharmpy model", {
  local_luna_project()
  mockery::stub(luna_run, "pharmr::read_model", list(dataset = data.frame(ID = 1)))
  expect_error(suppressMessages(luna_run("run1")), "not a pharmpy model")
})

test_that("luna_run aborts when model has no dataset", {
  local_luna_project()
  mockery::stub(luna_run, "pharmr::read_model", fake_pharmpy_model(dataset = NULL))
  expect_error(suppressMessages(luna_run("run1")), "no dataset")
})

test_that("luna_run aborts for nmfe method without nmfe path", {
  local_luna_project(config = run_config(list(method = "nmfe")))
  mockery::stub(luna_run, "pharmr::read_model", fake_pharmpy_model())
  expect_error(suppressMessages(luna_run("run1", nmfe = "  ")), "no nmfe path")
})

test_that("luna_run resolves nmfe location for nmfe method", {
  local_luna_project(config = run_config(list(method = "nmfe", nmfe = "/opt/nm/run/nmfe75")))
  mock_loc <- mockery::mock("/resolved/nmfe75")
  mock_run <- mockery::mock("fit")
  mockery::stub(luna_run, "pharmr::read_model", fake_pharmpy_model())
  local_mocked_bindings(get_nmfe_location = mock_loc, .package = "pharmr.extra")
  mockery::stub(luna_run, "pharmr.extra::run_nlme", mock_run)
  expect_equal(suppressMessages(luna_run("run1")), "fit")
  expect_equal(mockery::mock_args(mock_loc)[[1]][[1]], "/opt/nm/run/nmfe75")
  args <- mockery::mock_args(mock_run)[[1]]
  expect_equal(args$method, "nmfe")
  expect_equal(args$nmfe, "/resolved/nmfe75")
})

test_that("luna_run dispatches to pharmr.extra::run_nlme with the right args", {
  folder <- local_luna_project()
  model <- fake_pharmpy_model()
  mock_run <- mockery::mock("fit")
  mockery::stub(luna_run, "pharmr::read_model", model)
  mockery::stub(luna_run, "pharmr.extra::run_nlme", mock_run)
  suppressMessages(luna_run(1, nmfe = "/opt/nmfe", extra_arg = "x"))
  mockery::expect_called(mock_run, 1)
  args <- mockery::mock_args(mock_run)[[1]]
  expect_identical(args$model, model)
  expect_equal(args$id, "run1")
  expect_equal(args$path, folder)
  expect_equal(args$method, "pharmpy")
  expect_equal(args$nmfe, "/opt/nmfe")
  expect_false(args$as_job)
  expect_true(args$console)
  expect_equal(args$extra_arg, "x")
  expect_equal(.luna_cache$get("log")[[length(.luna_cache$get("log"))]]$action, "modelfit")
})

test_that("luna_run warns when modelfit method is not configured", {
  local_luna_project(config = run_config(list(console = FALSE)))
  mock_run <- mockery::mock("fit")
  mockery::stub(luna_run, "pharmr::read_model", fake_pharmpy_model())
  mockery::stub(luna_run, "pharmr.extra::run_nlme", mock_run)
  expect_message(luna_run("run1", nmfe = "/opt/nmfe"), "not configured")
  args <- mockery::mock_args(mock_run)[[1]]
  expect_equal(args$method, "pharmpy")
  expect_false(args$console)
})

test_that("luna_run dispatches to luna_run_ferx for ferx method", {
  folder <- local_luna_project(config = run_config(list(method = "ferx", data = "data.csv")))
  mock_ferx <- mockery::mock("ferx_result")
  mock_read <- mockery::mock()
  mockery::stub(luna_run, "luna_run_ferx", mock_ferx)
  mockery::stub(luna_run, "pharmr::read_model", mock_read)
  expect_equal(suppressMessages(luna_run("run1")), "ferx_result")
  args <- mockery::mock_args(mock_ferx)[[1]]
  expect_equal(args$id, "run1")
  expect_equal(args$folder, folder)
  expect_equal(args$config$tools$modelfit$method, "ferx")
  mockery::expect_called(mock_read, 0)
})
