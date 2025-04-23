library(gsm.datasim) # requires >= fix-50
library(gsm.mapping) # requires >= fix-58
library(gsm.core)

# generate gilda data for monitoring visit app:
ie_data <- generate_rawdata_for_single_study(
  SnapshotCount = 1,
  SnapshotWidth = "months",
  ParticipantCount = 1000,
  SiteCount = 50,
  StudyID = "ABC",
  workflow_path = "workflow",
  mappings = "IE",
  package = "gsm.mapping",
  desired_specs = NULL
)

lSource <- ie_data[[1]]
# Step 1 - Create Mapped Data Layer - filter, aggregate and join raw data to create mapped data layer
mappings_wf <- gsm.core::MakeWorkflowList(strNames = c("IE", "ENROLL"), strPath = "workflow/1_mappings", strPackage = "gsm.mapping")
mappings_spec <- gsm.mapping::CombineSpecs(mappings_wf)
lRaw <- gsm.mapping::Ingest(lSource, mappings_spec)
mapped <- gsm.core::RunWorkflows(mappings_wf, lRaw)

# Step 2 - Create Metrics - calculate metrics using mapped data
metrics_wf <- gsm.core::MakeWorkflowList(strPath = "inst/workflow/2_metrics", strPackage = "gsm.qtl")
analyzed <- gsm.core::RunWorkflows(metrics_wf, mapped)
