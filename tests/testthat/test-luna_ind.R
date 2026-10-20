setup_luna_ind <- function(.local_envir = parent.frame()) {
  skip_if_not_installed("ggplot2")
  local_luna_project(.local_envir = .local_envir)
}

test_that("luna_ind returns ggplot from run table and logs event", {
  setup_luna_ind()
  mock_tab <- mockery::mock(fake_nm_table(4))
  mockery::stub(luna_ind, "get_table_for_plots", mock_tab)
  expect_message(p <- luna_ind("run1", ncol = 2, nrow = 2), "page 1/1")
  expect_true(ggplot2::is_ggplot(p))
  expect_setequal(unique(p$data$name), c("PRED", "IPRED"))
  expect_true(mockery::mock_args(mock_tab)[[1]]$ltbs == FALSE)
  last <- log_get_last_event()
  expect_equal(last$event, "plot")
  expect_equal(last$context$plot, "ind")
})

test_that("luna_ind uses supplied data, shows SE ribbon and exponentiates", {
  setup_luna_ind()
  tab <- fake_nm_table(2)
  tab$IPRED_SE <- 0.1
  p <- luna_ind(
    "run1", data = tab, exponentiate = TRUE, se_factor = 2,
    ncol = 1, nrow = 2, verbose = FALSE
  )
  expect_true(ggplot2::is_ggplot(p))
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))))
  obs <- p$data[p$data$name == "IPRED", ]
  src <- tab[tab$MDV == 0, ]
  expect_equal(obs$value, exp(src$IPRED))
  expect_equal(obs$IPRED_min, exp(src$IPRED - 0.2))
})

test_that("luna_ind respects show settings", {
  setup_luna_ind()
  tab <- fake_nm_table(2)
  tab$IPRED_SE <- 0.1
  p <- luna_ind(
    "run1", data = tab, ncol = 1, nrow = 2, verbose = FALSE,
    show = list(pred = FALSE, doses = FALSE, dv = FALSE, se = FALSE)
  )
  expect_equal(unique(p$data$name), "IPRED")
  expect_length(p$layers, 1)
})

test_that("luna_ind aborts when page is not available", {
  setup_luna_ind()
  expect_error(
    luna_ind("run1", data = fake_nm_table(2), ncol = 1, nrow = 2, page = 2),
    "only 1 pages available"
  )
})

test_that("luna_ind plots only the selected subject_ids", {
  setup_luna_ind()
  p <- luna_ind(
    "run1", data = fake_nm_table(3), subject_ids = c(2, 3),
    ncol = 1, nrow = 2, verbose = FALSE
  )
  expect_setequal(unique(p$data$ID), c(2, 3))
})
