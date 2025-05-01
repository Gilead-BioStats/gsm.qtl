test_that("Analyze One Side Prop works",{
  test_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
       "ABC",     "Study",         25,          100,    0.25
  )

  res <- Analyze_OneSideProp(test_transformed, proprate = 0.01, 3)
  expect_true(all(c("Score", "Flag") %in% names(res)))
  expect_equal(res %>% filter(GroupID == "ABC") %>% pull(Flag), 2)
})
