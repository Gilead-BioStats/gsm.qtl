disc_plotly_trace_text <- function(plotly_obj) {
  unlist(
    lapply(plotly_obj$x$data, function(trace) {
      if (is.null(trace$text)) {
        character(0)
      } else {
        trace$text
      }
    }),
    use.names = FALSE
  )
}

disc_plotly_trace_names <- function(plotly_obj) {
  unlist(
    lapply(plotly_obj$x$data, function(trace) {
      if (is.null(trace$name)) {
        NA_character_
      } else {
        trace$name
      }
    }),
    use.names = FALSE
  )
}

test_that("discontinuation_groupBar uses all arguments", {
  df <- qtl_test_participant_df()
  out <- discontinuation_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site")
  out_custom <- discontinuation_groupBar(
    df = df,
    varGroupID = invid,
    strGroupLabel = "Site",
    varStatus = compyn,
    valuesDiscontinued = c("Y")
  )
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")
  expect_s3_class(out_custom, "plotly")

  default_text <- disc_plotly_trace_text(out)
  custom_text <- disc_plotly_trace_text(out_custom)

  expect_true(any(grepl("Discontinuation Status: Premature Discontinuation", default_text, fixed = TRUE)))
  expect_true(any(grepl("Discontinuation Status: Premature Discontinuation", custom_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Participant Count by Site", fixed = TRUE)
  expect_match(built$x$layout$annotations[[1]]$text, "Excludes site\\(s\\)")
})

test_that("discontinuation_reasonBar uses df and reason variable", {
  df <- qtl_test_participant_df()
  out <- discontinuation_reasonBar(df = df, varCompreas = compreas)
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  trace_names <- unique(stats::na.omit(disc_plotly_trace_names(out)))
  expect_true("Adverse event" %in% trace_names)
  expect_match(built$x$layout$title$text, "Participant Count by Reasons", fixed = TRUE)
})

test_that("reasons_groupBar uses group and reason arguments", {
  df <- qtl_test_participant_df()
  out <- reasons_groupBar(df = df, varGroupID = invid, varCompreas = compreas, strGroupLabel = "Site")
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  tooltip_text <- disc_plotly_trace_text(out)
  expect_true(any(grepl("Site: S01", tooltip_text, fixed = TRUE)))
  expect_true(any(grepl("Discontinuation Reason:", tooltip_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Discontinuation Reason by Site", fixed = TRUE)
})

test_that("discontinuation_map_reasons respects yaml_path", {
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
