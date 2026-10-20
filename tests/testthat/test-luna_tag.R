test_that("luna_tag aborts without luna cache", {
  local_no_luna_cache()
  expect_error(luna_tag("run1", "final"), "Luna cache not found")
})

test_that("luna_tag adds tags to the tag element", {
  dir <- local_luna_project()
  luna_tag("run2", "final")
  luna_tag("run2", "candidate")
  yml <- yaml::read_yaml(file.path(dir, "testproj.yaml"))
  expect_equal(yml$runs[[2]]$tag, c("final", "candidate"))
  expect_null(yml$runs[[2]]$notes)
  expect_equal(luna_tag("run2"), c("final", "candidate"))
})

test_that("luna_tag clears tags before adding", {
  dir <- local_luna_project()
  luna_tag("run1", "a")
  luna_tag("run1", "b", clear = TRUE)
  yml <- yaml::read_yaml(file.path(dir, "testproj.yaml"))
  expect_equal(yml$runs[[1]]$tag, "b")
})
