test_that("luna_clone copies model and adds run to YAML", {
  dir <- local_luna_project()
  edited <- NULL
  mockery::stub(luna_clone, "luna_edit", function(id) edited <<- id)
  msgs <- testthat::capture_messages(luna_clone("run2", 3))
  expect_true(any(grepl("New model run3 added", msgs)))
  expect_equal(edited, "run3")
  expect_equal(
    readLines(file.path(dir, "run3.mod")),
    readLines(file.path(dir, "run2.mod"))
  )
  yml <- yaml::read_yaml(file.path(dir, "testproj.yaml"))
  expect_equal(
    yml$runs[[3]],
    list(id = "run3", description = "Cloned from run2", reference = "run2")
  )
  expect_equal(pluck_entry(.luna_cache$get("project")$yaml$runs, "run3")$reference, "run2")
})

test_that("luna_clone aborts when target exists without force", {
  local_luna_project()
  mockery::stub(luna_clone, "luna_edit", function(id) NULL)
  expect_error(luna_clone("run1", "run2"), "use `force=TRUE` to overwrite")
})

test_that("luna_clone overwrites with force and warns when run already in YAML", {
  dir <- local_luna_project()
  mockery::stub(luna_clone, "luna_edit", function(id) NULL)
  msgs <- testthat::capture_messages(luna_clone("run1", "run2", force = TRUE))
  expect_true(any(grepl("exists, overwriting", msgs)))
  expect_equal(
    readLines(file.path(dir, "run2.mod")),
    readLines(file.path(dir, "run1.mod"))
  )
  expect_message(
    luna_clone("run1", "run2", force = TRUE, verbose = FALSE),
    "already exists in YAML"
  )
  yml <- yaml::read_yaml(file.path(dir, "testproj.yaml"))
  expect_length(yml$runs, 2)
  expect_equal(yml$runs[[2]]$description, "two-compartment")
})

test_that("luna_clone with update_inits calls PsN update_inits", {
  dir <- local_luna_project()
  mockery::stub(luna_clone, "luna_edit", function(id) NULL)
  psn <- mockery::mock(NULL)
  mockery::stub(luna_clone, "pharmr.extra::call_psn", psn)
  suppressMessages(luna_clone("run1", "run4", update_inits = TRUE))
  mockery::expect_called(psn, 1)
  mockery::expect_args(
    psn, 1,
    "run.mod",
    tool = "update_inits",
    path = file.path(dir, "run1"),
    options = list(output_model = "../run4.mod"),
    verbose = TRUE
  )
  yml <- yaml::read_yaml(file.path(dir, "testproj.yaml"))
  expect_equal(yml$runs[[3]]$id, "run4")
})
