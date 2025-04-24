library(gsm.datasim) # requires >= fix-50
library(gsm.mapping) # requires >= fix-58
library(gsm.reporting)
library(gsm.core)
devtools::load_all()
set.seed(1234)
# generate gilda data for monitoring visit app:
ie_data <- generate_rawdata_for_single_study(
  SnapshotCount = 6,
  SnapshotWidth = "months",
  ParticipantCount = 1000,
  SiteCount = 50,
  StudyID = "ABC",
  workflow_path = "workflow",
  mappings = "IE",
  package = "gsm.mapping",
  desired_specs = NULL
)

analyzed <- list()
reporting <- list()
dates <- names(ie_data) %>% as.Date
for(snap in seq_along(ie_data)){
  lSource <- ie_data[[snap]]

  mappings_wf <- gsm.core::MakeWorkflowList(strNames =c("SUBJ", "ENROLL", "IE", "STUDY", "SITE", "COUNTRY"), strPath = "workflow/1_mappings", strPackage = "gsm.mapping")
  mappings_spec <- gsm.mapping::CombineSpecs(mappings_wf)
  lRaw <- gsm.mapping::Ingest(lSource, mappings_spec)

  # Step 1 - Create Mapped Data Layer - filter, aggregate and join raw data to create mapped data layer
  mapped <- gsm.core::RunWorkflows(mappings_wf, lRaw)

  # Step 2 - Create Metrics - calculate metrics using mapped data
  metrics_wf <- gsm.core::MakeWorkflowList(strPath = "inst/workflow/2_metrics", strPackage = "gsm.qtl")
  analyzed[[snap]] <- gsm.core::RunWorkflows(metrics_wf, mapped)

  # # Step 3 - Create Reporting Layer - create reports using metrics data
  reporting_wf <- gsm.core::MakeWorkflowList(strPath = "workflow/3_reporting", strPackage = "gsm.reporting")
  reporting[[snap]] <- gsm.core::RunWorkflows(reporting_wf, c(mapped, list(lAnalyzed = analyzed[[snap]],
                                                                           lWorkflows = metrics_wf)))
  reporting[[snap]]$Reporting_Results$SnapshotDate = dates[snap]
  reporting[[snap]]$Reporting_Bounds$SnapshotDate = dates[snap]
}

all_reportingResults <- do.call(dplyr::bind_rows, lapply(reporting, function(x) x$Reporting_Results))
all_reportingGroups <- reporting[[snap]]$Reporting_Groups
all_reportingBounds <- do.call(dplyr::bind_rows, lapply(reporting, function(x) x$Reporting_Bounds))
all_reportingMetrics <- reporting[[snap]]$Reporting_Metrics

qtl_chart <- gsm.kri::Widget_TimeSeries(
  dfResults = all_reportingResults,
  dfGroups = all_reportingGroups
)
