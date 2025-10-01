testthat::test_that("Given appropriate inclusion/exclusion related data, calculates appropriate QTL threshold.", {

  expect_true(all(IE_outputs %in% names(analyzed_ineligibility[[1]])))

  # This is a study level metric
  expect_equal(nrow(analyzed_ineligibility[[1]]$Analysis_Transformed), 1)

  # Summary adds flag and score
  expect_equal(names(analyzed_ineligibility[[1]]$Analysis_Summary), c(names(analyzed_ineligibility[[1]]$Analysis_Transformed), "Flag", "Score"))

  nPropRate <- ineligibility_workflow$meta$nPropRate
  nNumDeviations <- ineligibility_workflow$meta$nNumDeviations

  flag_crit <- nPropRate + nNumDeviations*sqrt( (nPropRate*(1-nPropRate)) / analyzed_ineligibility[[1]]$Analysis_Summary$Denominator)
  actual_flag <- analyzed_ineligibility[[1]]$Analysis_Summary$Flag
  expected_flag <- case_when(analyzed_ineligibility[[1]]$Analysis_Summary$Metric >= flag_crit ~ 2,
                             analyzed_ineligibility[[1]]$Analysis_Summary$Metric < flag_crit & analyzed_ineligibility[[1]]$Analysis_Summary$Metric >= nPropRate ~ 1,
                             .default = 0)

  # The flagging should be working as expected
  expect_equal(actual_flag, expected_flag)
})
