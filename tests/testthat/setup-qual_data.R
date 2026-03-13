set.seed(123)

## default qtl path instead of inst/workflow
GetYamlPathDefaultMetrics <- function() {
  file.path(system.file(package = "gsm.qtl"), "workflow", "2_metrics")
}

# -------------------------------------------------------------------------
# Synthetic mapped data fixtures
# -------------------------------------------------------------------------

subject_base <- tibble::tibble(
  subjid = sprintf("SUBJ-%03d", seq_len(24)),
  subjectid = sprintf("SUBJ-%03d", seq_len(24)),
  studyid = "ABC",
  invid = rep(c("S01", "S02", "S03", "S04"), each = 6),
  country = rep(c("US", "CA", "GB", "DE"), each = 6)
)

mapped_exclusion <- subject_base %>%
  mutate(
    Source = case_when(
      row_number() %% 5 == 0 ~ "Neither",
      row_number() %% 2 == 0 ~ "Eligibility IPD only",
      TRUE ~ "EDC"
    ),
    eligibility_criteria = ifelse(Source == "Neither", "", "Criterion A"),
    ie_violation = ifelse(Source == "Neither", "N", "Y")
  ) %>%
  select(subjid, subjectid, Source, studyid, invid, country, eligibility_criteria, ie_violation)

mapped_subj <- subject_base %>%
  select(subjid, studyid, invid)

disc_reason_cycle <- c("Withdrew Consent", "Lost to Follow-Up", "Death", "PHYSICIAN DECISION")

mapped_studcomp <- subject_base %>%
  mutate(
    compyn = ifelse(row_number() %% 4 == 0, "N", "Y"),
    compreas = ifelse(
      compyn == "N",
      disc_reason_cycle[((row_number() - 1) %% length(disc_reason_cycle)) + 1],
      "Completed"
    )
  ) %>%
  select(studyid, subjid, compyn, compreas)

mapped <- list(
  snapshot_1 = list(
    Mapped_EXCLUSION = mapped_exclusion,
    Mapped_SUBJ = mapped_subj,
    Mapped_STUDCOMP = mapped_studcomp
  )
)

# -------------------------------------------------------------------------
# Workflow execution helper
# -------------------------------------------------------------------------

run_workflow_outputs <- function(
  lWorkflow,
  lData,
  steps = seq_along(lWorkflow$steps),
  bKeepInputData = FALSE
) {
  workflow <- lWorkflow

  if (length(workflow$steps) == 0) {
    cli::cli_abort("Workflow has no steps")
  }

  workflow$lData <- lData

  if ("spec" %in% names(workflow)) {
    gsm.core::CheckSpec(workflow$lData, workflow$spec)
  }

  if (length(steps) > 1) {
    workflow$steps <- workflow$steps[steps]
  } else if (length(steps) == 1) {
    workflow$steps <- list(workflow$steps[[steps]])
  }

  for (step in workflow$steps) {
    result <- gsm.core::RunStep(
      lStep = step,
      lData = workflow$lData,
      lMeta = workflow$meta
    )

    workflow$lData[[step$output]] <- result
    workflow$lResult <- result
  }

  if (!bKeepInputData) {
    output_names <- purrr::map_chr(workflow$steps, ~ .x$output)
    workflow$lData <- workflow$lData[output_names]
  }

  workflow$lData
}

# -------------------------------------------------------------------------
# Test setup objects consumed by test-qual_T*.R
# -------------------------------------------------------------------------

ineligibility_workflow <- purrr::flatten(
  gsm.core::MakeWorkflowList(
    strNames = c("qtl0001"),
    strPath = GetYamlPathDefaultMetrics()
  )
)

discontinuation_workflow <- purrr::flatten(
  gsm.core::MakeWorkflowList(
    strNames = c("qtl0002"),
    strPath = GetYamlPathDefaultMetrics()
  )
)

analyzed_ineligibility <- purrr::map_depth(
  mapped,
  1,
  ~ run_workflow_outputs(ineligibility_workflow, .x, bKeepInputData = FALSE)
)

analyzed_discontinuation <- purrr::map_depth(
  mapped,
  1,
  ~ run_workflow_outputs(discontinuation_workflow, .x, bKeepInputData = FALSE)
)

IE_outputs <- purrr::map_vec(ineligibility_workflow$steps, ~ .x$output)
SDSC_outputs <- purrr::map_vec(discontinuation_workflow$steps, ~ .x$output)
