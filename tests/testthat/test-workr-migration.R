# Qualification test for gsm.qtl#120.
#
# The workflow-runtime functions were re-pointed from {gsm.core} to {workr}.
# Staying functions (Input_*, Transform_*, Flag_*, Summarize, ParseThreshold)
# intentionally remain on gsm.core::, so this test only checks the extracted
# runtime surface.

test_that("workflow specs route extracted runtime functions through workr (#120)", {
  skip_if_not_installed("yaml")

  wf_dir <- system.file("workflow", package = "gsm.qtl")
  expect_false(identical(wf_dir, ""))

  yaml_files <- list.files(
    wf_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_gt(length(yaml_files), 0)

  step_functions <- unlist(lapply(yaml_files, function(f) {
    steps <- tryCatch(yaml::read_yaml(f)$steps, error = function(e) NULL)
    if (is.null(steps)) {
      return(character(0))
    }
    step_keys <- c("name", "names")
    unlist(
      lapply(steps, function(s) unlist(s[step_keys], use.names = FALSE)),
      use.names = FALSE
    )
  }))
  step_functions <- step_functions[!is.na(step_functions)]

  # Extracted runtime functions must resolve from {workr}, not {gsm.core}.
  expect_false("gsm.core::RunQuery" %in% step_functions)

  # ...and the re-point actually happened (RunQuery is the SQL/dplyr primitive).
  expect_true("workr::RunQuery" %in% step_functions)
})
