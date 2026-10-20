test_that("luna_note aborts without id", {
  expect_error(luna_note(NULL), "specify an `id`")
})

test_that("luna_note aborts without luna cache", {
  local_no_luna_cache()
  expect_error(luna_note("run1", "a note"), "Luna cache not found")
})

test_that("luna_note returns NULL with message when no notes exist", {
  local_luna_project()
  expect_message(res <- luna_note("run1"), "No entry found for run1")
  expect_null(res)
})

test_that("luna_note adds notes and writes them to YAML", {
  dir <- local_luna_project()
  luna_note("run1", "first note")
  luna_note(1, "second note")
  yml <- yaml::read_yaml(file.path(dir, "testproj.yaml"))
  expect_equal(yml$runs[[1]]$notes, c("first note", "second note"))
  expect_equal(
    pluck_entry(.luna_cache$get("project")$yaml$runs, "run1")$notes,
    c("first note", "second note")
  )
  expect_equal(luna_note("run1"), c("first note", "second note"))
  expect_null(yml$runs[[2]]$notes)
})

test_that("luna_note clears existing notes before adding new one", {
  dir <- local_luna_project()
  luna_note("run1", "old note")
  msgs <- testthat::capture_messages(
    luna_note("run1", "new note", clear = TRUE, verbose = TRUE)
  )
  expect_true(any(grepl("notes updated for run1", msgs)))
  yml <- yaml::read_yaml(file.path(dir, "testproj.yaml"))
  expect_equal(yml$runs[[1]]$notes, "new note")
})

test_that("luna_note uses custom element", {
  dir <- local_luna_project()
  luna_note("run2", "abc", element = "remarks")
  yml <- yaml::read_yaml(file.path(dir, "testproj.yaml"))
  expect_equal(yml$runs[[2]]$remarks, "abc")
})
