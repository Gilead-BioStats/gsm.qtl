test_that("Analyze One Side Prop works for Study", {
  test_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
    "ABC", "Study", 25, 100, 0.25
  )

  res <- Analyze_OneSideProp(test_transformed, nPropRate = 0.01, nNumDeviations = 3)

  expect_equal(nrow(res), 1)
})
