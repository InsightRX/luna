test_that("nm_save_model stops with error if `modelfile` is not provided", {
  expect_error(
    nm_save_model(model = list(), modelfile = NULL, overwrite = FALSE), 
    "Please specify an output NONMEM modelfile."
  )
})

test_that("nm_save_model stops with error if `model` is not provided", {
  expect_error(
    nm_save_model(model = NULL, modelfile = "model.txt", overwrite = FALSE), 
    "Please specify an imported NONMEM model"
  )
})

test_that("nm_save_model stops with error if file already exists and `overwrite` is set to FALSE", {
  # Create a dummy file
  file.create("dummy.txt")
  expect_error(
    nm_save_model(model = list(), modelfile = "dummy.txt", overwrite = FALSE), 
    "Sorry, the output NONMEM file already exists and `overwrite` is set to FALSE."
  )
  
  # Remove the dummy file
  file.remove("dummy.txt")
})

test_that("nm_save_model stops with error if `model` is not a NONMEM model", {
  expect_error(
    nm_save_model(model = list(), modelfile = "model.txt", overwrite = TRUE), 
    "Sorry, this object does not seem to be a valid NONMEM model object."
  )
})

test_that("nm_save_model writes a file with the correct content", {
  # Create a dummy NONMEM model
  model <- structure(
    list(PROBLEM = "problem", INPUT = "input", DATA = "data", ABBR = "abbr"), 
    class = "NONMEM"
  )
  nm_save_model(model = model, modelfile = "test.txt", overwrite = TRUE)
  expect_equal(readLines("test.txt"), c("problem", "input", "data", "abbr"))
  file.remove("test.txt")
})
