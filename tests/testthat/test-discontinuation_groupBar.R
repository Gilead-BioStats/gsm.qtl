test_that("discontinuation_groupBar default status logic remains unchanged", {
  test_df <- data.frame(
    site = c("A", "A", "B", "B", "C", "C"),
    compyn = c("Y", "Y", "N", "Y", "Y", "Y"),
    alt_status = c("C", "D", "C", "C", "D", "C")
  )

  plot_obj <- discontinuation_groupBar(test_df, varGroupID = site, strGroupLabel = "Site")
  plot_data <- plotly::plotly_build(plot_obj)$x$data
  plotted_groups <- plot_data |>
    lapply(function(trace) trace$text) |>
    unlist() |>
    as.character()
  plotted_groups <- sub(".*Site: ([^<]+).*", "\\1", plotted_groups)
  plotted_groups <- unique(plotted_groups)

  expect_setequal(plotted_groups, "B")
})


test_that("discontinuation_groupBar supports custom status variable and values", {
  test_df <- data.frame(
    site = c("A", "A", "B", "B", "C", "C"),
    compyn = c("Y", "Y", "N", "Y", "Y", "Y"),
    alt_status = c("C", "D", "C", "C", "D", "C")
  )

  plot_obj <- discontinuation_groupBar(
    test_df,
    varGroupID = site,
    strGroupLabel = "Site",
    varStatus = alt_status,
    valuesDiscontinued = "D"
  )

  plot_data <- plotly::plotly_build(plot_obj)$x$data
  plotted_groups <- plot_data |>
    lapply(function(trace) trace$text) |>
    unlist() |>
    as.character()
  plotted_groups <- sub(".*Site: ([^<]+).*", "\\1", plotted_groups)
  plotted_groups <- unique(plotted_groups)

  expect_setequal(plotted_groups, c("A", "C"))
})
