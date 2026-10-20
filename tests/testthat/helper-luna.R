#' Point HOME to a temp folder containing a luna config file
#'
#' Keeps tests from reading or writing the user's real
#' `~/.config/luna/config.yaml`.
#'
#' @param config list to write as config. Defaults to the package's
#' `inst/config/default.yaml`, with `as_job` disabled.
#' @param .local_envir environment that controls the lifetime of the temp HOME
#' @return Path to the temp HOME folder
local_luna_home <- function(config = NULL, .local_envir = parent.frame()) {
  home <- withr::local_tempdir(pattern = "luna_home_", .local_envir = .local_envir)
  withr::local_envvar(HOME = home, .local_envir = .local_envir)
  dir.create(file.path(home, ".config", "luna"), recursive = TRUE)
  if (is.null(config)) {
    config <- yaml::read_yaml(
      system.file("config", "default.yaml", package = "luna")
    )
    config$general$runs$as_job <- FALSE
  }
  if (!isFALSE(config)) {
    yaml::write_yaml(config, file.path(home, ".config", "luna", "config.yaml"))
  }
  home
}

#' Make sure `.luna_cache` is absent from the global env during a test, and
#' restore whatever was there afterwards.
#'
#' @param .local_envir environment that controls the lifetime of the change
local_no_luna_cache <- function(.local_envir = parent.frame()) {
  genv <- rlang::global_env()
  had_cache <- exists(".luna_cache", envir = genv, inherits = FALSE)
  old_cache <- if (had_cache) get(".luna_cache", envir = genv) else NULL
  if (had_cache) rm(".luna_cache", envir = genv)
  withr::defer({
    if (exists(".luna_cache", envir = genv, inherits = FALSE)) {
      rm(".luna_cache", envir = genv)
    }
    if (had_cache) assign(".luna_cache", old_cache, envir = genv)
  }, envir = .local_envir)
  invisible()
}

#' Write a minimal NONMEM control stream
#'
#' @param path file to write
#' @param problem text for the `$PROBLEM` record
write_nm_model <- function(path, problem = "test model") {
  writeLines(c(
    paste("$PROBLEM", problem),
    "$INPUT ID TIME AMT DV MDV EVID",
    "$DATA data.csv IGNORE=@",
    "$SUBROUTINE ADVAN1 TRANS2",
    "$PK",
    "CL = THETA(1) * EXP(ETA(1))",
    "V  = THETA(2)",
    "S1 = V",
    "$ERROR",
    "Y = F + F * EPS(1)",
    "$THETA (0, 10) (0, 50)",
    "$OMEGA 0.1",
    "$SIGMA 0.1",
    "$ESTIMATION METHOD=1 INTERACTION"
  ), path)
  invisible(path)
}

#' Create a temporary NONMEM luna project and load it into the cache
#'
#' Creates a temp folder with NONMEM model files (`run1.mod`, `run2.mod`, ...),
#' a small dataset, and a project YAML, sets up a temp HOME with a luna config,
#' and loads the project with `luna_load_project()` so that `.luna_cache` is
#' available. Everything is cleaned up when the calling test finishes.
#'
#' @param runs list of run entries for the project YAML. Model files are
#' created for each run id.
#' @param name project name
#' @param config luna config list passed to `local_luna_home()`
#' @param load load the project into the cache?
#' @param .local_envir environment that controls the lifetime of the fixture
#' @return Path to the temp project folder
local_luna_project <- function(
  runs = list(
    list(id = "run1", description = "base model"),
    list(id = "run2", description = "two-compartment", reference = "run1")
  ),
  name = "testproj",
  config = NULL,
  load = TRUE,
  .local_envir = parent.frame()
) {
  local_luna_home(config = config, .local_envir = .local_envir)
  local_no_luna_cache(.local_envir = .local_envir)
  dir <- withr::local_tempdir(pattern = "luna_proj_", .local_envir = .local_envir)
  dir <- normalizePath(dir)
  for (run in runs) {
    write_nm_model(file.path(dir, paste0(run$id, ".mod")), run$description)
  }
  writeLines(c(
    "ID,TIME,AMT,DV,MDV,EVID",
    "1,0,100,0,1,1",
    "1,1,0,5,0,0",
    "1,4,0,2,0,0"
  ), file.path(dir, "data.csv"))
  yaml::write_yaml(
    list(project = list(description = "test project"), runs = runs),
    file.path(dir, paste0(name, ".yaml"))
  )
  if (load) {
    suppressMessages(luna_load_project(name = name, folder = dir, verbose = FALSE))
  }
  dir
}

#' Data.frame resembling a NONMEM output table, for plotting tests
#'
#' @param n_id number of subjects
fake_nm_table <- function(n_id = 2) {
  do.call(rbind, lapply(seq_len(n_id), function(i) {
    data.frame(
      ID = i,
      TIME = c(0, 1, 2, 4, 8),
      EVID = c(1, 0, 0, 0, 0),
      MDV = c(1, 0, 0, 0, 0),
      DV = c(0, 5, 4, 3, 1) * i,
      PRED = c(0, 4.8, 4.1, 2.9, 1.2) * i,
      IPRED = c(0, 5.1, 3.9, 3.1, 1.1) * i,
      CWRES = c(0, 0.2, -0.1, 0.3, -0.2),
      NPDE = c(0, 0.1, -0.2, 0.2, -0.1)
    )
  }))
}
