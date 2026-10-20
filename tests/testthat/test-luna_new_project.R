local_new_project_folder <- function(.local_envir = parent.frame()) {
  local_luna_home(.local_envir = .local_envir)
  local_no_luna_cache(.local_envir = .local_envir)
  normalizePath(withr::local_tempdir(.local_envir = .local_envir))
}

test_that("luna_new_project imports existing models in numeric order", {
  dir <- local_new_project_folder()
  write_nm_model(file.path(dir, "run10.mod"), "ten")
  write_nm_model(file.path(dir, "run2.mod"), "two")
  write_nm_model(file.path(dir, "run1.mod"), "one")
  expect_message(
    luna_new_project("myproj", folder = dir, description = "my project"),
    "Done"
  )
  yml <- yaml::read_yaml(file.path(dir, "myproj.yaml"))
  expect_equal(yml$project$description, "my project")
  expect_equal(vapply(yml$runs, `[[`, "", "id"), c("run1", "run2", "run10"))
  expect_equal(vapply(yml$runs, `[[`, "", "description"), c("one", "two", "ten"))
  expect_equal(.luna_cache$get("name"), "myproj")
  expect_s3_class(.luna_cache$get("project"), "luna.project")
})

test_that("luna_new_project creates the folder if needed", {
  parent <- local_new_project_folder()
  dir <- file.path(parent, "newproj")
  expect_message(
    luna_new_project("myproj", folder = dir, description = "x"),
    "Creating folder"
  )
  expect_true(file.exists(file.path(dir, "myproj.yaml")))
})

test_that("luna_new_project aborts when parent folder is missing", {
  parent <- local_new_project_folder()
  expect_error(
    luna_new_project("p", folder = file.path(parent, "a", "b"), description = "x"),
    "does not exist"
  )
})

test_that("luna_new_project aborts on existing project unless forced", {
  dir <- local_new_project_folder()
  write_nm_model(file.path(dir, "run1.mod"), "one")
  writeLines("project: {}", file.path(dir, "myproj.yaml"))
  expect_error(
    luna_new_project("myproj", folder = dir, description = "x", verbose = FALSE),
    "Project file exists"
  )
  suppressMessages(
    luna_new_project("myproj", folder = dir, description = "new", force = TRUE, verbose = FALSE)
  )
  expect_equal(yaml::read_yaml(file.path(dir, "myproj.yaml"))$project$description, "new")
})

test_that("get_description_from_model extracts $PROBLEM text", {
  f <- withr::local_tempfile()
  writeLines(c("$PROBLEM  my model ", "$INPUT ID"), f)
  expect_equal(get_description_from_model(f), "my model")
  writeLines(c("$PROB", "next line desc", "$INPUT ID"), f)
  expect_equal(get_description_from_model(f), "next line desc")
  writeLines(c("$INPUT ID", "$PK"), f)
  expect_equal(get_description_from_model(f), "No description available")
  writeLines(c("$INPUT ID", "$PROBLEM"), f)
  expect_equal(get_description_from_model(f), "N/A")
})

test_that("save_project writes yaml and respects force", {
  f <- withr::local_tempfile(fileext = ".yaml")
  project <- list(yaml = list(project = list(description = "d")))
  expect_message(save_project(project, f, verbose = TRUE), "Saving project")
  expect_equal(yaml::read_yaml(f), project$yaml)
  expect_error(save_project(project, f))
  project$yaml$project$description <- "e"
  save_project(project, f, force = TRUE)
  expect_equal(yaml::read_yaml(f)$project$description, "e")
})

test_that("luna_new_project with import_models = FALSE creates project without runs", {
  dir <- local_new_project_folder()
  write_nm_model(file.path(dir, "run1.mod"), "one")
  suppressMessages(
    luna_new_project("myproj", folder = dir, description = "x", import_models = FALSE)
  )
  yml <- yaml::read_yaml(file.path(dir, "myproj.yaml"))
  expect_null(yml$runs)
})

test_that("luna_new_project works in a folder without models", {
  dir <- local_new_project_folder()
  suppressMessages(luna_new_project("myproj", folder = dir, description = "x"))
  yml <- yaml::read_yaml(file.path(dir, "myproj.yaml"))
  expect_length(yml$runs, 0)
})

test_that("luna_new_project creates project from template", {
  dir <- local_new_project_folder()
  suppressMessages(
    luna_new_project("myproj", folder = dir, description = "tmpl", template = "clean.yaml")
  )
  yml <- yaml::read_yaml(file.path(dir, "myproj.yaml"))
  expect_equal(yml$project$description, "tmpl")
  expect_equal(.luna_cache$get("name"), "myproj")
  expect_equal(.luna_cache$get("project")$metadata$folder, dir)
})
