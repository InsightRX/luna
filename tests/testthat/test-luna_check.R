fake_pharmpy_model <- function(dataset = data.frame(ID = 1)) {
  structure(list(dataset = dataset), class = "pharmpy.model.model.Model")
}

test_that("luna_check aborts for multiple ids", {
  expect_error(luna_check(c("run1", "run2")), "multiple runs")
})

test_that("luna_check aborts when model file is missing", {
  local_luna_project()
  expect_error(suppressMessages(luna_check("run9")), "not found")
})

test_that("luna_check returns FALSE when model cannot be read", {
  local_luna_project()
  mockery::stub(luna_check, "pharmr::read_model", function(...) stop("bad syntax"))
  expect_message(res <- luna_check("run1"), "bad syntax")
  expect_false(res)
})

test_that("luna_check returns FALSE for non-pharmpy model", {
  local_luna_project()
  mockery::stub(luna_check, "pharmr::read_model", list(dataset = data.frame(ID = 1)))
  expect_message(res <- luna_check("run1"), "could not be parsed")
  expect_false(res)
})

test_that("luna_check returns FALSE when model has no dataset", {
  local_luna_project()
  mockery::stub(luna_check, "pharmr::read_model", fake_pharmpy_model(dataset = NULL))
  expect_message(res <- luna_check("run1"), "dataset could not be loaded")
  expect_false(res)
})

test_that("luna_check returns FALSE when NONMEM check fails", {
  local_luna_project()
  mockery::stub(luna_check, "pharmr::read_model", fake_pharmpy_model())
  mockery::stub(
    luna_check, "pharmr.extra::run_nlme",
    structure(FALSE, message = "compilation error\n")
  )
  expect_output(res <- suppressMessages(luna_check("run1")), "compilation error")
  expect_false(res)
})

test_that("luna_check returns TRUE on successful check", {
  folder <- local_luna_project()
  mock_run <- mockery::mock(TRUE)
  mockery::stub(luna_check, "pharmr::read_model", fake_pharmpy_model())
  mockery::stub(luna_check, "pharmr.extra::run_nlme", mock_run)
  expect_message(res <- luna_check("run1", verbose = TRUE), "syntax OK")
  expect_true(res)
  args <- mockery::mock_args(mock_run)[[1]]
  expect_equal(args$id, "run1")
  expect_equal(args$path, folder)
  expect_equal(args$method, "pharmpy")
  expect_true(args$check_only)
  expect_true(args$verbose)
})
