test_that("discontinuation_groupBar uses all arguments (#14, #21, #22, #76, #90)", {
  df <- qtl_test_participant_df()
  dfNum <- df %>% dplyr::filter(compyn == "N")
  out <- discontinuation_groupBar(dfNum = dfNum, dfDenom = df, varGroupID = invid, strGroupLabel = "Site")
  out_custom <- discontinuation_groupBar(
    dfNum = dfNum,
    dfDenom = df,
    varGroupID = invid,
    strGroupLabel = "Site",
    varStatus = compyn,
    valuesDiscontinued = c("Y", "N", "N", "Y")
  )
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")
  expect_s3_class(out_custom, "plotly")

  default_text <- plotly_trace_text(out)
  custom_text <- plotly_trace_text(out_custom)
  annotations <- built$x$layout[["annotations"]]

  expect_true(any(grepl("Discontinuation Status: Premature Discontinuation", default_text, fixed = TRUE)))
  expect_true(any(grepl("Discontinuation Status: Premature Discontinuation", custom_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Participant Count by Site", fixed = TRUE)
  expect_match(annotations[[1]][["text"]], "Excludes .* site\\(s\\)")
  expect_match(annotations[[1]][["text"]], "prematurely discontinued participants")
  expect_equal(annotations[[1]][["yanchor"]], "top")
  expect_lt(annotations[[1]][["yshift"]], 0)
  expect_gt(built$x$layout$margin$b, 50)
})

test_that("discontinuation_reasonBar uses df and reason variable (#14, #21, #22)", {
  df <- qtl_test_participant_df()
  out <- discontinuation_reasonBar(df = df, varCompreas = compreas)
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  trace_names <- unique(stats::na.omit(plotly_trace_names(out)))
  expect_true("Adverse event" %in% trace_names)
  expect_match(built$x$layout$title$text, "Participant Count by Reasons", fixed = TRUE)
})

test_that("reasons_groupBar uses group and reason arguments (#14, #21, #22)", {
  df <- qtl_test_participant_df()
  out <- reasons_groupBar(df = df, varGroupID = invid, varCompreas = compreas, strGroupLabel = "Site")
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  tooltip_text <- plotly_trace_text(out)
  expect_true(any(grepl("Site: S01", tooltip_text, fixed = TRUE)))
  expect_true(any(grepl("Discontinuation Reason:", tooltip_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Discontinuation Reason by Site", fixed = TRUE)
})

test_that("reasons_groupBar swaps axes correctly when bSwapAxes is TRUE (#90)", {
  df <- qtl_test_participant_df()
  out <- reasons_groupBar(df = df, varGroupID = invid, varCompreas = compreas, strGroupLabel = "Site", bSwapAxes = TRUE)
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  tooltip_text <- plotly_trace_text(out)
  expect_true(any(grepl("Site:", tooltip_text, fixed = TRUE)))
  expect_true(any(grepl("Discontinuation Reason:", tooltip_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Site by Discontinuation Reason", fixed = TRUE)
})

test_that("discontinuation_map_reasons respects yaml_path (#21, #22)", {
  df <- qtl_test_participant_df()

  yaml_file <- tempfile(fileext = ".yaml")
  writeLines(
    c(
      "steps:",
      "  - params:",
      "      reasons:",
      "        - Adverse event",
      "        - Withdrawal"
    ),
    con = yaml_file
  )

  out <- discontinuation_map_reasons(df = df, yaml_path = yaml_file)

  expect_s3_class(out, "data.frame")
  expect_true(all(out$compreas %in% c("Adverse event", "Withdrawal")))
  expect_false("Protocol deviation" %in% out$compreas)
})
