test_that("ifelse0 returns value or alternative", {
  expect_equal(ifelse0(1, 2), 1)
  expect_equal(ifelse0(NULL, 2), 2)
  expect_null(ifelse0(NULL, NULL, allow_null = TRUE))
  expect_error(ifelse0(NULL), "No alternative")
})

test_that("pluck_entry finds an entry by id", {
  x <- list(list(id = "a", v = 1), list(id = "b", v = 2))
  expect_equal(pluck_entry(x, "b")$v, 2)
  expect_null(pluck_entry(x, "c"))
  expect_equal(pluck_entry(x, 1, el = "v")$id, "a")
})

test_that("insert_entry replaces matching entry", {
  x <- list(list(id = "a", v = 1), list(id = "b", v = 2))
  out <- insert_entry(x, "b", list(id = "b", v = 3))
  expect_equal(out[[2]]$v, 3)
  expect_equal(out[[1]]$v, 1)
})

test_that("get_time_ago returns readable difference or empty string", {
  expect_equal(get_time_ago(NA), "")
  expect_match(get_time_ago(lubridate::now() - 3600 * 2), "^2 hours$")
})

test_that("get_time_last_updated_file returns ISO stamp or empty string", {
  f <- withr::local_tempfile(lines = "x")
  expect_match(get_time_last_updated_file(f), "^\\d{4}-\\d{2}-\\d{2}T")
  expect_equal(get_time_last_updated_file(file.path(tempdir(), "nope.txt")), "")
})

test_that("get_time_last_updated_folder returns ISO stamp or empty string", {
  dir <- withr::local_tempdir()
  expect_equal(get_time_last_updated_folder(dir), "")
  writeLines("x", file.path(dir, "a.txt"))
  expect_match(get_time_last_updated_folder(dir), "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("find_file_with_fallback finds file, fallback, or aborts", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "main.txt"))
  writeLines("x", file.path(dir, "fb.txt"))
  expect_equal(
    find_file_with_fallback(dir, "main.txt", "fb.txt"),
    file.path(dir, "main.txt")
  )
  expect_message(
    out <- find_file_with_fallback(dir, "none.txt", "fb.txt"),
    "fallback"
  )
  expect_equal(out, file.path(dir, "fb.txt"))
  expect_silent(find_file_with_fallback(dir, "none.txt", "fb.txt", verbose = FALSE))
  expect_error(find_file_with_fallback(dir, "none.txt", "none2.txt"), "not found")
  expect_null(find_file_with_fallback(dir, "none.txt", "none2.txt", abort = FALSE))
})

test_that("read_yaml_safe parses `n` keys", {
  f <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c("a:", "  n: 5", "  m: 2"), f)
  out <- read_yaml_safe(f)
  expect_equal(out$a$n, 5)
  expect_equal(out$a$m, 2)
})

test_that("is_run_as_job uses config unless overridden", {
  conf <- list(general = list(runs = list(as_job = TRUE)))
  expect_true(is_run_as_job(conf))
  expect_false(is_run_as_job(conf, as_job = FALSE))
  expect_false(is_run_as_job(list()))
})

test_that("get_flag_from_config reads flag from given or global config", {
  conf <- list(tools = list(modelfit = list(force = TRUE)))
  expect_true(get_flag_from_config(config = conf))
  expect_false(get_flag_from_config(c("tools", "other"), config = conf))
  local_luna_home(config = conf)
  local_no_luna_cache()
  expect_true(suppressMessages(get_flag_from_config()))
})

test_that("replace_list_elements replaces and warns on unknown names", {
  x <- list(a = 1, b = 2)
  expect_equal(replace_list_elements(x, list(b = 3)), list(a = 1, b = 3))
  expect_warning(
    out <- replace_list_elements(x, list(a = 5, z = 1)),
    "Nothing named"
  )
  expect_equal(out, list(a = 5, b = 2))
  expect_warning(
    replace_list_elements(x, list(y = 1, z = 1)),
    "^Nothing named: y, z found to replace$"
  )
})

test_that("validate_id converts numeric ids and aborts on NULL", {
  expect_equal(validate_id(3), "run3")
  expect_equal(validate_id("abc"), "abc")
  expect_error(validate_id(NULL), "specify an `id`")
})

test_that("get_tools_info finds tool folders for a run", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "run1", "modelfit_dir1"), recursive = TRUE)
  dir.create(file.path(dir, "run1", "other"))
  out <- get_tools_info("run1", dir, tools = c("modelfit", "bootstrap"))
  expect_equal(out, list(modelfit = "modelfit_dir1"))
  expect_equal(get_tools_info("run1", dir), list())
})
