test_that("eligibility_groupBar uses all arguments (#14, #15, #21, #22, #60)", {
  df <- qtl_test_participant_df()
  dfNum <- df %>% dplyr::filter(Source != "Neither")

  out_counts <- eligibility_groupBar(dfNum = dfNum, dfDenom = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = FALSE)
  out_perc <- eligibility_groupBar(dfNum = dfNum, dfDenom = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = TRUE)
  built_counts <- plotly::plotly_build(out_counts)
  built_perc <- plotly::plotly_build(out_perc)

  expect_s3_class(out_counts, "plotly")
  expect_s3_class(out_perc, "plotly")

  count_text <- plotly_trace_text(out_counts)
  perc_text <- plotly_trace_text(out_perc)
  annotations <- built_counts$x$layout[["annotations"]]

  expect_true(any(grepl("Eligibility Status: Ineligible", count_text, fixed = TRUE)))
  expect_true(any(grepl("Site: S01", count_text, fixed = TRUE)))
  expect_true(any(grepl("Percentage:", perc_text, fixed = TRUE)))
  expect_match(built_counts$x$layout$title$text, "Participant Count by Site", fixed = TRUE)
  expect_match(built_perc$x$layout$title$text, "Participant Percentage by Site", fixed = TRUE)
  expect_null(annotations)
  expect_equal(built_counts$x$layout$margin$b, 50)
})

test_that("eligibility_groupBar reserves space when a footnote is present (#90)", {
  df <- dplyr::bind_rows(
    qtl_test_participant_df(),
    tibble::tribble(
      ~invid, ~country, ~subjid, ~Source, ~ietestcd_concat, ~dvdtm, ~eligibility_criteria, ~compyn, ~compreas,
      "S04", "US", "SUBJ-007", "Neither", "", "2024-04-01", "", "Y", "",
      "S04", "US", "SUBJ-008", "Neither", "", "2024-04-02", "", "", ""
    )
  )
  dfNum <- df %>% dplyr::filter(Source != "Neither")

  out <- eligibility_groupBar(dfNum = dfNum, dfDenom = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = FALSE)
  annotations <- out$x$layoutAttrs[[1]][["annotations"]]
  margins <- out$x$layoutAttrs[[1]][["margin"]]

  expect_match(annotations[[1]][["text"]], "Excludes .* site\\(s\\)")
  expect_match(annotations[[1]][["text"]], "no ineligible participants")
  expect_equal(annotations[[1]][["yanchor"]], "top")
  expect_lt(annotations[[1]][["yshift"]], 0)
  expect_gt(margins[["b"]], 50)
})

test_that("eligibility_groupBar bPercentage = FALSE shows counts (#60)", {
  df <- qtl_test_participant_df()
  dfNum <- df %>% dplyr::filter(Source != "Neither")
  out <- eligibility_groupBar(dfNum = dfNum, dfDenom = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = FALSE)
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")
  expect_match(built$x$layout$xaxis$title$text, "Participant Count", fixed = TRUE)
  expect_match(built$x$layout$title$text, "Participant Count by Site", fixed = TRUE)
})

test_that("eligibility_groupBar bPercentage = TRUE shows percentages (#60)", {
  df <- qtl_test_participant_df()
  dfNum <- df %>% dplyr::filter(Source != "Neither")
  out <- eligibility_groupBar(dfNum = dfNum, dfDenom = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = TRUE)
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")
  expect_match(built$x$layout$xaxis$title$text, "Participant Percentage", fixed = TRUE)
  expect_match(built$x$layout$title$text, "Participant Percentage by Site", fixed = TRUE)
})

test_that("eligibility_groupBar bPercentage TRUE uses stacked fill position (#60)", {
  df <- qtl_test_participant_df()
  dfNum <- df %>% dplyr::filter(Source != "Neither")
  out_counts <- eligibility_groupBar(dfNum = dfNum, dfDenom = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = FALSE)
  out_perc <- eligibility_groupBar(dfNum = dfNum, dfDenom = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = TRUE)
  built_counts <- plotly::plotly_build(out_counts)
  built_perc <- plotly::plotly_build(out_perc)

  # Percentage chart should have bars that reach 1.0 (100%) across all groups
  perc_x_values <- unlist(lapply(built_perc$x$data, function(trace) max(trace$x, na.rm = TRUE)))
  expect_true(all(perc_x_values <= 1.0 + 1e-6))  # Allow for small floating point errors

  # Count chart may have varying max x values
  count_x_values <- unlist(lapply(built_counts$x$data, function(trace) max(trace$x, na.rm = TRUE)))
  expect_true(any(count_x_values > 1.0))  # At least one should be > 1.0 for counts
})

test_that("eligibility_sourceBar returns plotly object (#14, #21, #22)", {
  df <- qtl_test_participant_df()
  out <- eligibility_sourceBar(df = df)
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  source_text <- plotly_trace_text(out)

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
  expect_true(any(grepl("Criteria:", criteria_text, fixed = TRUE)))
  expect_true(any(grepl("Site:", criteria_text, fixed = TRUE)))
  expect_true(any(grepl("Criteria: I001", criteria_text, fixed = TRUE)))
  expect_true(any(grepl("Criteria: E010", criteria_text, fixed = TRUE)))
  expect_false(any(grepl("Criteria: I001, E010", criteria_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Criteria by Site", fixed = TRUE)
})

test_that("criteria_groupBar uses grouping and label arguments correctly when swapping axes (#90)", {
  df <- qtl_test_participant_df() %>%
    dplyr::mutate(ietestcd_concat = gsub(";;;", ",", ietestcd_concat, fixed = TRUE))

  out <- criteria_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site", bSwapAxes = TRUE)
  built <- plotly::plotly_build(out)

  expect_s3_class(out, "plotly")

  criteria_text <- plotly_trace_text(out)
  expect_true(any(grepl("Criteria:", criteria_text, fixed = TRUE)))
  expect_true(any(grepl("Site:", criteria_text, fixed = TRUE)))
  expect_match(built$x$layout$title$text, "Site by Criteria", fixed = TRUE)
})

test_that("eligibility_listing covers df and download arguments (#21, #22, #24, #25)", {
  df <- qtl_test_participant_df()

  out_download <- eligibility_listing(df = df, download = TRUE)
  out_gt <- eligibility_listing(df = df, download = FALSE)

  expect_s3_class(out_download, "data.frame")
  expect_true(inherits(out_gt, "shiny.tag") || inherits(out_gt, "shiny.tag.list"))
  expect_true(all(c("Country", "Site", "Participant ID", "Which I/E") %in% names(out_download)))
  expect_true(nrow(out_download) > 0)
})

test_that("eligibility_listing handles zero-row data frame (#108)", {
  df <- qtl_test_participant_df()[0, ]

  out_download <- eligibility_listing(df = df, download = TRUE)
  out_gt <- eligibility_listing(df = df, download = FALSE)

  expect_s3_class(out_download, "data.frame")
  expect_equal(nrow(out_download), 0)
  expect_s3_class(out_gt, "gt_tbl")
})

test_that("eligibility_listing handles all-NA dvdtm and eligibility_criteria (#108)", {
  df <- tibble::tribble(
    ~invid, ~country, ~subjid, ~Source, ~ietestcd_concat, ~dvdtm, ~eligibility_criteria, ~compyn, ~compreas,
    "S01", "US", "SUBJ-001", "EDC", "I001", NA_character_, NA_character_, "N", "AE"
  )

  out <- eligibility_listing(df = df, download = TRUE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_true("PD Date1" %in% names(out))
  expect_true("PD Term1" %in% names(out))
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
