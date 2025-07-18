# Test Setup -------------------------------------------------------
earlydisc_workflow <- flatten(
  gsm.core::MakeWorkflowList(
    strNames = c("qtl0002"),
    strPath = yaml_path_custom_metrics,
    strPackage = "gsm.qtl"
  )
)

# define Data ------------------------------------------------------
analyzed_earlydisc <- purrr::map_depth(mapped, 1, ~robust_runworkflow(earlydisc_workflow, .x, bKeepInputData = FALSE))

## define outputs --------------------------------------------------
outputs <- map_vec(ineligibility_workflow$steps, ~ .x$output)

## Test Code -------------------------------------------------------
testthat::test_that("Given appropriate mapped participant-level data, calculates appropriate QTL threshold.", {

  expect_true(all(outputs %in% names(analyzed_earlydisc[[1]])))
  expect_true(all(map_lgl(analyzed_earlydisc[[1]][outputs[!outputs %in% c("lAnalysis")]], is.data.frame)))

  # This is a study level metric
  expect_equal(nrow(analyzed_earlydisc[[1]]$Analysis_Transformed), 1)

  # Summary adds a row for Upper funnel and Flatline
  expect_equal(nrow(analyzed_earlydisc[[1]]$Analysis_Summary), 3)

  # flatline value matches `nPropRate` in yaml
  expect_equal(pull(filter(analyzed_earlydisc[[1]]$Analysis_Summary, GroupID == "Flatline"), Metric), earlydisc_workflow$meta$nPropRate)
})
