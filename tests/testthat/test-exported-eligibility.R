test_that("eligibility_groupBar uses all arguments (#14, #15, #21, #22, #60)", {
  df <- qtl_test_participant_df()

  out_counts <- eligibility_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = FALSE)
  out_perc <- eligibility_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = TRUE)
  built_counts <- plotly::plotly_build(out_counts)
  built_perc <- plotly::plotly_build(out_perc)

  expect_s3_class(out_counts, "plotly")
  expect_s3_class(out_perc, "plotly")

  count_text <- plotly_trace_text(out_counts)
  perc_text <- plotly_trace_text(out_perc)

  expect_true(any(grepl("Eligibility Status: Ineligible", count_text, fixed = TRUE)))
  expect_true(any(grepl("Site: S01", count_text, fixed = TRUE)))
  expect_true(any(grepl("Percentage:", perc_text, fixed = TRUE)))
  expect_match(built_counts$x$layout$title$text, "Participant Count by Site", fixed = TRUE)
  expect_match(built_perc$x$layout$title$text, "Participant Percentage by Site", fixed = TRUE)
  expect_match(built_counts$x$layout$annotations[[1]]$text, "Excludes site\\(s\\)")
})

test_that("eligibility_sourceBar returns plotly object (#14, #21, #22)", {
  df <- qtl_test_participant_df()
  out <- eligibility_sourceBar(df = df)
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  trace_names <- unique(stats::na.omit(plotly_trace_names(out)))
  source_text <- plotly_trace_text(out)

  expect_false("Neither" %in% trace_names)
  expect_true(any(grepl("Source: EDC", source_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Participant Count by Category/Source", fixed = TRUE)
})

test_that("criteria_groupBar uses grouping and label arguments (#14, #21, #22, #23)", {
  df <- qtl_test_participant_df() %>%
    dplyr::mutate(ietestcd_concat = gsub(";;;", ",", ietestcd_concat, fixed = TRUE))

  out <- criteria_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site")
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  criteria_text <- plotly_trace_text(out)
  expect_true(any(grepl("Eligibility Status: I001", criteria_text, fixed = TRUE)))
  expect_true(any(grepl("Eligibility Status: E010", criteria_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Eligibility by Site", fixed = TRUE)
})

test_that("eligibility_listing covers df and download arguments (#21, #22, #24, #25)", {
  df <- qtl_test_participant_df()

  out_download <- eligibility_listing(df = df, download = TRUE)
  out_gt <- eligibility_listing(df = df, download = FALSE)

  expect_s3_class(out_download, "data.frame")
  expect_true(inherits(out_gt, "shiny.tag") || inherits(out_gt, "shiny.tag.list"))
  expect_true(all(c("Site", "Country", "Subject ID", "Which I/E") %in% names(out_download)))
  expect_true(nrow(out_download) > 0)
})

test_that("scrollable_gt uses height and width arguments (#21, #22)", {
  gt_tbl <- gt::gt(head(qtl_test_participant_df(), 2))
  out <- scrollable_gt(gt_tbl = gt_tbl, height = "200px", min_table_width = "800px")

  expect_true(inherits(out, "shiny.tag") || inherits(out, "shiny.tag.list"))
  out_html <- as.character(out)
  expect_match(out_html, "max-height: 200px", fixed = TRUE)
  expect_match(out_html, "min-width: 800px", fixed = TRUE)
})

test_that("Eligibility_Overview uses all arguments (#21, #22, #70)", {
  df_results <- qtl_test_results_df()

  out <- Eligibility_Overview(
    dfResults = df_results,
    dSnapshot = as.Date("2024-01-01"),
    strNum = "Numerator N",
    strDenom = "Denominator N",
    strQTL = "Eligibility Rate"
  )

  expect_s3_class(out, "gt_tbl")
})
