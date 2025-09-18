test_that("Analyze One Side Prop works for Study, when nProRate is clearly violated", {
  test_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
    "ABC", "Study", 25, 100, 0.25
  )
  nPropRate <- 0.01
  nNumDeviations <- 3

  res <- Analyze_OneSideProp(test_transformed, nPropRate = nPropRate, nNumDeviations = nNumDeviations)
  # There should only be 1 QTL Metric per study
  expect_equal(nrow(res), 1)

  flag_crit <- nPropRate + 3*sqrt( (nPropRate*(1-nPropRate)) / res$Denominator)
  actual_flag <- res$Flag
  expected_flag <- case_when(res$Metric >= flag_crit ~ 2,
                             res$Metric < flag_crit & res$Metric > nPropRate ~ 1,
                             .default = 0)

  # The flagging should be working as expected
  expect_equal(actual_flag, expected_flag)
})


test_that("Analyze One Side Prop works for Study, when nPropRate is the same", {
  test_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
    "ABC", "Study", 25, 100, 0.25
  )
  nPropRate <- 0.25
  nNumDeviations <- 1

  res <- Analyze_OneSideProp(test_transformed, nPropRate = nPropRate, nNumDeviations = nNumDeviations)
  # There should only be 1 QTL Metric per study
  expect_equal(nrow(res), 1)

  flag_crit <- nPropRate + 3*sqrt( (nPropRate*(1-nPropRate)) / res$Denominator)
  actual_flag <- res$Flag
  expected_flag <- case_when(res$Metric >= flag_crit ~ 2,
                             res$Metric < flag_crit & res$Metric >= nPropRate ~ 1,
                             .default = 0)

  # The flagging should be working as expected
  expect_equal(actual_flag, expected_flag)
})
