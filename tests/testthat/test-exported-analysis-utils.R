test_that("Analyze_OneSideProp handles required and optional arguments (#80)", {
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

test_that("Analyze_OneSideProp validates nPropRate argument bounds (#80)", {
  df_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
    "ABC", "Study", 25, 100, 0.25
  )

  expect_error(
    Analyze_OneSideProp(dfTransformed = df_transformed, nPropRate = 1, nNumDeviations = 3),
    "between 0 and 1"
  )
})

test_that("calc_fig_size covers n_rows, base, and per arguments (#80)", {
  expect_equal(calc_fig_size(n_rows = 10), 500L)
  expect_equal(calc_fig_size(n_rows = 12, base = 100, per = 50), 600L)
  expect_error(calc_fig_size(n_rows = -1), "single non-negative")
})

test_that("calc_plotly_footnote_layout expands bottom margin for wrapped footnotes", {
  layout <- gsm.qtl:::calc_plotly_footnote_layout(
    paste(rep("footnote", 30), collapse = " "),
    margins = list(l = 40, r = 40, b = 50, t = 40),
    wrap_width = 20
  )

  expect_gt(layout$margin$b, 50)
  expect_equal(layout$annotations[[1]]$x, 0)
  expect_equal(layout$annotations[[1]]$y, 0)
  expect_equal(layout$annotations[[1]]$yanchor, "top")
  expect_lt(layout$annotations[[1]]$yshift, 0)
  expect_match(layout$annotations[[1]]$text, "<br>")
})

test_that("ResultsProcessor uses dfResults and dfMetrics arguments (#80)", {
  inputs <- qtl_test_processor_inputs()

  out <- ResultsProcessor(
    dfResults = inputs$dfResults,
    dfMetrics = inputs$dfMetrics
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(c("Upper_funnel", "Flatline") %in% unique(out$GroupID)))
  expect_true(all(c("nPropRate", "nNumDeviations") %in% names(out)))
})