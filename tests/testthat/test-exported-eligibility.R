test_that("eligibility_groupBar uses all arguments", {
  df <- qtl_test_participant_df()

  out_counts <- eligibility_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = FALSE)
  out_perc <- eligibility_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site", bPercentage = TRUE)

  expect_s3_class(out_counts, "plotly")
  expect_s3_class(out_perc, "plotly")
})

test_that("eligibility_sourceBar returns plotly object", {
  df <- qtl_test_participant_df()
  out <- eligibility_sourceBar(df = df)
  expect_s3_class(out, "plotly")
})

test_that("criteria_groupBar uses grouping and label arguments", {
  df <- qtl_test_participant_df()
  out <- criteria_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site")
  expect_s3_class(out, "plotly")
})

test_that("eligibility_listing covers df and download arguments", {
  df <- qtl_test_participant_df()

  out_download <- eligibility_listing(df = df, download = TRUE)
  out_gt <- eligibility_listing(df = df, download = FALSE)

  expect_s3_class(out_download, "data.frame")
  expect_true(inherits(out_gt, "shiny.tag") || inherits(out_gt, "shiny.tag.list"))
})

test_that("scrollable_gt uses height and width arguments", {
  gt_tbl <- gt::gt(head(qtl_test_participant_df(), 2))
  out <- scrollable_gt(gt_tbl = gt_tbl, height = "200px", min_table_width = "800px")

  expect_true(inherits(out, "shiny.tag") || inherits(out, "shiny.tag.list"))
})

test_that("Eligibility_Overview uses all arguments", {
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