test_that("discontinuation_groupBar uses all arguments", {
  df <- qtl_test_participant_df()
  out <- discontinuation_groupBar(df = df, varGroupID = invid, strGroupLabel = "Site")

  expect_s3_class(out, "plotly")
})

test_that("discontinuation_reasonBar uses df and reason variable", {
  df <- qtl_test_participant_df()
  out <- discontinuation_reasonBar(df = df, varCompreas = compreas)

  expect_s3_class(out, "plotly")
})

test_that("reasons_groupBar uses group and reason arguments", {
  df <- qtl_test_participant_df()
  out <- reasons_groupBar(df = df, varGroupID = invid, varCompreas = compreas, strGroupLabel = "Site")

  expect_s3_class(out, "plotly")
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
})