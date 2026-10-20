test_that("print.luna.run_table prints runs in numeric order", {
  x <- data.frame(id = c("run10", "run2", "run1"), description = c("c", "b", "a"))
  class(x) <- c("luna.run_table", class(x))
  out <- capture.output(expect_message(print(x, verbose = TRUE), "Loading models"))
  rows <- grep("^run", out, value = TRUE)
  expect_equal(sub("\\s.*", "", rows), c("run1", "run2", "run10"))
  expect_output(print(x, format = "pipe"), "\\|run1 ")
})

test_that("print.luna.run_table handles empty table", {
  x <- data.frame(id = character(0))
  class(x) <- c("luna.run_table", class(x))
  expect_message(print(x, verbose = TRUE), "No models found")
  expect_silent(print(x))
})

test_that("truncate_columns truncates flexible columns to fit width", {
  df <- data.frame(
    id = c("run1", "run2"),
    desc = c(strrep("a", 50), "short"),
    notes = c(strrep("b", 50), NA),
    empty = c(NA, NA)
  )
  specs <- c(id = NA, desc = 2, notes = 1, empty = NA)
  out <- truncate_columns(df, specs, total_width = 34)
  expect_equal(out$id, df$id)
  expect_equal(nchar(out$desc[1]), 20)
  expect_equal(out$desc[2], "short")
  expect_equal(nchar(out$notes[1]), 10)
  withr::local_options(width = 34)
  expect_equal(truncate_columns(df, specs), out)
})

test_that("truncate_columns aborts when console is too narrow", {
  df <- data.frame(id = "run1", desc = "abc")
  expect_error(
    truncate_columns(df, c(id = NA, desc = 1), total_width = 4),
    "does not fit"
  )
})

test_that("luna_project_templates lists available templates", {
  expect_output(luna_project_templates(), "Available templates:.*- clean")
  out <- capture.output(luna_project_templates())
  expect_true(" - clean" %in% out)
  expect_false(any(grepl("\\\\n", out, fixed = TRUE)))
})
