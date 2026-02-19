test_that("Analyze_OneSideProp handles required and optional arguments", {
  df_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
    "ABC", "Study", 25, 100, 0.25
  )

  out_default <- Analyze_OneSideProp(dfTransformed = df_transformed)
  out_custom <- Analyze_OneSideProp(
    dfTransformed = df_transformed,
    nPropRate = 0.20,
    nNumDeviations = 2
  )

  expect_s3_class(out_default, "data.frame")
  expect_s3_class(out_custom, "data.frame")
  expect_named(out_custom, c("GroupID", "GroupLevel", "Numerator", "Denominator", "Metric", "Flag", "Score"))
  expect_equal(nrow(out_custom), 1)
  expect_true(out_custom$Flag %in% c(0, 1, 2))
})

test_that("Analyze_OneSideProp validates nPropRate argument bounds", {
  df_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
    "ABC", "Study", 25, 100, 0.25
  )

  expect_error(
    Analyze_OneSideProp(dfTransformed = df_transformed, nPropRate = 1, nNumDeviations = 3),
    "between 0 and 1"
  )
})

test_that("calc_fig_size covers n_rows, base, and per arguments", {
  expect_equal(calc_fig_size(n_rows = 10), 500L)
  expect_equal(calc_fig_size(n_rows = 12, base = 100, per = 50), 600L)
  expect_error(calc_fig_size(n_rows = -1), "single non-negative")
})

test_that("ResultsProcessor uses dfResults and dfMetrics arguments", {
  inputs <- qtl_test_processor_inputs()

  out <- ResultsProcessor(
    dfResults = inputs$dfResults,
    dfMetrics = inputs$dfMetrics
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(c("Upper_funnel", "Flatline") %in% unique(out$GroupID)))
  expect_true(all(c("nPropRate", "nNumDeviations") %in% names(out)))
})