set.seed(123)
library(dplyr)
library(gsm.datasim)
library(gsm.qtl)
library(purrr)

## Declare all the data
ie_data <- generate_rawdata_for_single_study(
  SnapshotCount = 1,
  SnapshotWidth = "months",
  ParticipantCount = 1000,
  SiteCount = 10,
  StudyID = "ABC",
  workflow_path = "workflow/1_mappings",
  mappings = c("IE", "PD", "STUDCOMP"),
  package = "gsm.mapping",
  desired_specs = NULL
)

## custom kris path instead of inst/workflow
yaml_path_custom_mappings <- "tests/testqualification/qualification/qual_workflows/1_mappings"
yaml_path_custom_metrics <- "tests/testqualification/qualification/qual_workflows/2_metrics"

mappings_wf <- gsm.core::MakeWorkflowList(
  strNames =c("SUBJ", "ENROLL", "IE", "PD", "STUDY", "SITE", "COUNTRY", "EXCLUSION", "STUDCOMP"),
  strPath = yaml_path_custom_mappings,
  strPackage = "gsm.qtl"
)

mappings_spec <- gsm.mapping::CombineSpecs(mappings_wf)
lRaw <- purrr::map_depth(ie_data, 1, gsm.mapping::Ingest, mappings_spec)
mapped <- purrr::map_depth(lRaw, 1, ~ gsm.core::RunWorkflows(mappings_wf, .x))

# mappings_spec <- gsm.mapping::CombineSpecs(mappings_wf)
metrics_wf <- gsm.core::MakeWorkflowList(
  strNames = c("qtl0001", "qtl0002"),
  strPath = yaml_path_custom_metrics,
  strPackage = "gsm.qtl"
)

analyzed <- purrr::map_depth(mapped, 1, ~gsm.core::RunWorkflows(metrics_wf, .x))

# Robust version of Runworkflow no config that will always run even with errors, and can be specified for specific steps in workflow to run
robust_runworkflow <- function(
  lWorkflow,
  lData,
  steps = seq(lWorkflow$steps),
  bReturnResult = TRUE,
  bKeepInputData = TRUE
) {
  # Create a unique identifier for the workflow
  uid <- paste0(lWorkflow$meta$Type, "_", lWorkflow$meta$ID)
  cli::cli_h1("Initializing `{uid}` Workflow")

  # check that the workflow has steps
  if (length(lWorkflow$steps) == 0) {
    cli::cli_alert("Workflow `{uid}` has no `steps` property.")
  }

  if (!"meta" %in% names(lWorkflow)) {
    cli::cli_alert("Workflow `{uid}` has no `meta` property.")
  }

  lWorkflow$lData <- lData

  # If the workflow has a spec, check that the data and spec are compatible
  if ("spec" %in% names(lWorkflow)) {
    cli::cli_h3("Checking data against spec")
    CheckSpec(lData, lWorkflow$spec)
  } else {
    lWorkflow$spec <- NULL
    cli::cli_h3("No spec found in workflow. Proceeding without checking data.")
  }

  if (length(steps) > 1) {
    lWorkflow$steps <- lWorkflow$steps[steps]
  } else if (length(steps) == 1) {
    lWorkflow$steps <- list(lWorkflow$steps[[steps]])
  }


  # Run through each steps in lWorkflow$workflow
  stepCount <- 1
  for (steps in lWorkflow$steps) {
    cli::cli_h2(paste0("Workflow steps ", stepCount, " of ", length(lWorkflow$steps), ": `", steps$name, "`"))

    result0 <- purrr::safely(
      ~ gsm.core::RunStep(
        lStep = steps,
        lData = lWorkflow$lData,
        lMeta = lWorkflow$meta
      )
    )()
    if (names(result0[!map_vec(result0, is.null)]) == "error") {
      cli::cli_alert_danger(paste0("Error:`", result0$error$message, "`: ", "error message stored as result"))
      result1 <- result0$error$message
    } else {
      result1 <- result0$result
    }

    lWorkflow$lData[[steps$output]] <- result1
    lWorkflow$lResult <- result1

    if (is.data.frame(result1)) {
      cli::cli_h3("{paste(dim(result1),collapse='x')} data.frame saved as `lData${steps$output}`.")
    } else {
      cli::cli_h3("{typeof(result1)} of length {length(result1)} saved as `lData${steps$output}`.")
    }

    stepCount <- stepCount + 1
  }


  # Return the result of the last step (the default) or the full workflow

  if (!bKeepInputData) {
    outputs <- lWorkflow$steps %>% purrr::map_chr(~ .x$output)
    lWorkflow$lData <- lWorkflow$lData[outputs]
    # cli::cli_alert_info("Returning workflow outputs: {names(lWorkflow$lData)}")
  } else {
    # cli::cli_alert_info("Returning workflow inputs and outputs: {names(lWorkflow$lData)}")
  }

  if (bReturnResult) {
    return(lWorkflow$lData)
  } else {
    return(lWorkflow)
  }
}
