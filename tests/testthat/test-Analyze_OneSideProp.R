test_that("Analyze One Side Prop works for Study",{
  test_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
       "ABC",     "Study",         25,          100,    0.25
  )

  res <- Analyze_OneSideProp(test_transformed, proprate = 0.01, 3)
  expect_equal(c(names(test_transformed), all(c("Score", "Flag")), names(res)))
  expect_equal(res %>% filter(GroupID == "ABC") %>% pull(Flag), 2)
})

test_that("Analyze One Side Prop works for Site",{
  test_transformed <- tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
     "SiteX",      "Site",         30,          100,    0.30,
     "SiteY",      "Site",         28,          100,    0.28,
     "SiteZ",      "Site",          2,          100,    0.02
  )

  res <- Analyze_OneSideProp(test_transformed, proprate = 0.01, 3)
  expect_equal(nrow(res), 4)
  expect_equal(res %>% pull(Flag), c(2,2,0,2))
  expect_equal(res %>% filter(GroupID == "Upper_funnel") %>% pull(Metric),
               0.01 + 3*sqrt(0.01*0.99/300))
})


