test_that("QTL_lineplot uses dfResults and strQTL arguments", {
  df_results <- qtl_test_results_df()

  out <- QTL_lineplot(dfResults = df_results, strQTL = "QTL Rate")
  expect_s3_class(out, "plotly")
})

test_that("QTL_lineplot_v2 returns timeseries widget with threshold flags", {
  df_results <- qtl_test_results_df()

  out <- QTL_lineplot_v2(dfResults = df_results, strQTL = "QTL Rate")

  expect_s3_class(out, "htmlwidget")
  expect_s3_class(out, "Widget_TimeSeriesQTL")

  df_payload <- jsonlite::fromJSON(out$x$dfResults)
  expect_true(all(c("StudyA", "Upper_funnel", "flat_line") %in% unique(df_payload$GroupID)))
  expect_true(all(c("Above QTL Threshold", "Below QTL Threshold") %in% unique(stats::na.omit(df_payload$Flag))))

  l_metric <- jsonlite::fromJSON(out$x$lMetric)
  expect_equal(l_metric$Abbreviation, "QTL Rate")
  expect_equal(l_metric$GroupLevel, "Study")
  expect_true("StudyA" %in% l_metric$selectedGroupIDs)
})

test_that("QTL_lineplot_v2 validates required arguments", {
  expect_error(
    QTL_lineplot_v2(dfResults = NULL, strQTL = "QTL Rate"),
    "dfResults is not a data.frame",
    fixed = TRUE
  )

  expect_error(
    QTL_lineplot_v2(dfResults = qtl_test_results_df(), strQTL = c("A", "B")),
    "strQTL must be a length-1 character",
    fixed = TRUE
  )
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

test_that("Report_QTL validates required structures", {
  report_params <- qtl_test_report_params()
  template <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "output: html_document", "---", "test"), con = template)

  expect_error(
    Report_QTL(
      dfResults = NULL,
      dfMetrics = report_params$dfMetrics,
      dfGroups = report_params$dfGroups,
      lListings = report_params$lListings,
      strOutputDir = tempdir(),
      strOutputFile = "qtl-report-test.html",
      strInputPath = template
    ),
    "dfResults must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    Report_QTL(
      dfResults = dplyr::select(report_params$dfResults, -MetricID),
      dfMetrics = report_params$dfMetrics,
      dfGroups = report_params$dfGroups,
      lListings = report_params$lListings,
      strOutputDir = tempdir(),
      strOutputFile = "qtl-report-test.html",
      strInputPath = template
    ),
    "dfResults is missing required column(s): MetricID",
    fixed = TRUE
  )

  expect_error(
    Report_QTL(
      dfResults = report_params$dfResults,
      dfMetrics = report_params$dfMetrics,
      dfGroups = report_params$dfGroups,
      lListings = list(data.frame(study = "StudyA", stringsAsFactors = FALSE)),
      strOutputDir = tempdir(),
      strOutputFile = "qtl-report-test.html",
      strInputPath = template
    ),
    "lListings[[1]] must be a data.frame containing studyid",
    fixed = TRUE
  )
})

test_that("Report_QTL passes arguments to render", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  report_params <- qtl_test_report_params(include_metric_ids = "Analysis_qtl0001")
  df_results <- report_params$dfResults
  df_metrics <- report_params$dfMetrics
  df_groups <- report_params$dfGroups
  l_listings <- report_params$lListings

  captured <- NULL
  template <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "output: html_document", "---", "test"), con = template)

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
    dfMetrics = df_metrics,
    dfGroups = df_groups,
    lListings = l_listings,
    strOutputDir = tmp_dir,
    strOutputFile = out_file,
    strInputPath = template
  )

  expect_equal(out, file.path(tmp_dir, out_file))
  expect_equal(captured$output_file, file.path(tmp_dir, out_file))
  expect_equal(captured$params$dfResults, df_results)
  expect_equal(captured$params$dfMetrics, df_metrics)
  expect_equal(captured$params$dfGroups, df_groups)
  expect_equal(captured$params$lListings, l_listings)
})

test_that("Report_QTL renders key visualization and UX sections", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("base64enc")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is required for report rendering tests")

  report_params <- qtl_test_report_params()
  tmp_dir <- tempfile("qtl-report-")
  dir.create(tmp_dir, recursive = TRUE)

  out <- Report_QTL(
    dfResults = report_params$dfResults,
    dfMetrics = report_params$dfMetrics,
    dfGroups = report_params$dfGroups,
    lListings = report_params$lListings,
    strOutputDir = tmp_dir,
    strOutputFile = "qtl-report-test.html"
  )

  expect_true(file.exists(out))

  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "QTL Overview", fixed = TRUE)
  expect_match(html, "QTL0001 - Ineligibility", fixed = TRUE)
  expect_match(html, "QTL0002 - Premature Study Discontinuation", fixed = TRUE)
  expect_match(html, "Inclusion/Exclusion Listing", fixed = TRUE)
  expect_match(html, "Discontinuation Listing", fixed = TRUE)
  expect_match(html, "eligibility_listing\\.csv")
  expect_match(html, "discontinuation_listing\\.csv")
  expect_match(html, "Widget_TimeSeriesQTL", fixed = TRUE)

  expect_true(
    stringr::str_detect(html, "plotly html-widget") ||
      stringr::str_detect(html, "plotly-graph-div")
  )
  expect_gte(stringr::str_count(html, "gt_table"), 2)
})

test_that("Report_QTL conditionally excludes metric sections", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("base64enc")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is required for report rendering tests")

  report_params <- qtl_test_report_params(include_metric_ids = "Analysis_qtl0001")
  tmp_dir <- tempfile("qtl-report-")
  dir.create(tmp_dir, recursive = TRUE)

  out <- Report_QTL(
    dfResults = report_params$dfResults,
    dfMetrics = report_params$dfMetrics,
    dfGroups = report_params$dfGroups,
    lListings = report_params$lListings,
    strOutputDir = tmp_dir,
    strOutputFile = "qtl-report-single-metric.html"
  )

  expect_true(file.exists(out))

  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "QTL0001 - Ineligibility", fixed = TRUE)
  expect_no_match(html, "QTL0002 - Premature Study Discontinuation", fixed = TRUE)
})