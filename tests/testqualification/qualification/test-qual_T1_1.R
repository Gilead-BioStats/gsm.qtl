test_that("Analyze One Side Prop works for Study", {
  test_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
    "ABC", "Study", 25, 100, 0.25
  )

  res <- Analyze_OneSideProp(test_transformed, nPropRate = 0.01, nNumDeviations = 3)

  expect_equal(nrow(res), 3) #calculates an upper funnel and flatline for each study
  expect_equal(pull(filter(res, GroupID == "Upper_funnel"), Metric), (0.01 + 3*sqrt(0.01*0.99/100)))
  expect_equal(pull(filter(res, GroupID == "Upper_funnel"), Flag), 2)
})
