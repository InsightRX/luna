test_that("get_table_for_plots selects first table with required columns", {
  local_luna_project()
  tab <- fake_nm_table()
  mock_tables <- mockery::mock(list(sdtab = tab[, c("ID", "TIME")], patab = tab))
  mockery::stub(get_table_for_plots, "luna_tables", mock_tables)
  res <- get_table_for_plots("run1", verbose = FALSE)
  expect_equal(res, tab)
  expect_equal(mockery::mock_args(mock_tables)[[1]]$folder, .luna_cache$get("project")$metadata$folder)
})

test_that("get_table_for_plots aborts when no table has required columns", {
  tab <- fake_nm_table()
  tab$NPDE <- NULL
  mockery::stub(get_table_for_plots, "luna_tables", list(tab))
  expect_error(
    get_table_for_plots(1, folder = "/proj", residual = "NPDE"),
    "Could not find a table"
  )
})

test_that("get_table_for_plots exponentiates observations for ltbs", {
  tab <- fake_nm_table(1)
  mockery::stub(get_table_for_plots, "luna_tables", list(tab))
  res <- get_table_for_plots("run1", folder = "/proj", ltbs = TRUE)
  obs <- tab$EVID == 0
  expect_equal(res$DV[obs], exp(tab$DV[obs]))
  expect_equal(res$PRED[obs], exp(tab$PRED[obs]))
  expect_equal(res$IPRED[obs], exp(tab$IPRED[obs]))
  expect_equal(res$DV[!obs], tab$DV[!obs])
})
