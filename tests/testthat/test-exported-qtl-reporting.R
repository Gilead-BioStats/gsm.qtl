test_that("QTL_lineplot uses dfResults and strQTL arguments", {
  df_results <- qtl_test_results_df()

  out <- QTL_lineplot(dfResults = df_results, strQTL = "QTL Rate")
  expect_s3_class(out, "plotly")
})

test_that("QTL_Overview uses all arguments", {
  df_results <- qtl_test_results_df()

  out <- QTL_Overview(
    dfResults = df_results,
    dSnapshot = as.Date("2024-01-01"),
    strNum = "Numerator N",
    strDenom = "Denominator N",
    strQTL = "QTL Rate"
  )

  expect_s3_class(out, "gt_tbl")
})

test_that("Report_QTL passes arguments to render", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  df_results <- qtl_test_results_df()
  inputs <- qtl_test_processor_inputs()
  df_groups <- data.frame(GroupID = "StudyA", stringsAsFactors = FALSE)
  l_listings <- list(eligibility = head(qtl_test_participant_df(), 2))

  captured <- NULL

  testthat::local_mocked_bindings(
    check_installed = function(...) NULL,
    .package = "rlang"
  )
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, intermediates_dir, envir) {
      captured <<- list(
        input = input,
        output_file = output_file,
        params = params,
        intermediates_dir = intermediates_dir,
        envir = envir
      )
      output_file
    },
    .package = "rmarkdown"
  )

  tmp_dir <- tempdir()
  out_file <- "qtl-report-test.html"

  out <- Report_QTL(
    dfResults = df_results,
    dfMetrics = inputs$dfMetrics,
    dfGroups = df_groups,
    lListings = l_listings,
    strOutputDir = tmp_dir,
    strOutputFile = out_file,
    strInputPath = tempfile(fileext = ".Rmd")
  )

  expect_equal(out, file.path(tmp_dir, out_file))
  expect_equal(captured$output_file, file.path(tmp_dir, out_file))
  expect_equal(captured$params$dfResults, df_results)
  expect_equal(captured$params$dfMetrics, inputs$dfMetrics)
  expect_equal(captured$params$dfGroups, df_groups)
  expect_equal(captured$params$lListings, l_listings)
})