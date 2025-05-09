library(gsm.datasim) # requires >= fix-50 PR to get it to dev is up
library(gsm.mapping) # dev is fine, as long as post fix-58
library(gsm.reporting)
library(gsm.core)
library(gsm.kri)
library(purrr)
library(dplyr)
devtools::load_all()
set.seed(1234)

# Single Study
ie_data <- generate_rawdata_for_single_study(
  SnapshotCount = 6,
  SnapshotWidth = "months",
  ParticipantCount = 1000,
  SiteCount = 10,
  StudyID = "ABC",
  workflow_path = "workflow",
  mappings = "IE",
  package = "gsm.mapping",
  desired_specs = NULL
)

mappings_wf <- gsm.core::MakeWorkflowList(strNames =c("SUBJ", "ENROLL", "IE", "STUDY", "SITE", "COUNTRY"), strPath = "workflow/1_mappings", strPackage = "gsm.mapping")
mappings_spec <- gsm.mapping::CombineSpecs(mappings_wf)
metrics_wf <- gsm.core::MakeWorkflowList(strNames = c("qtl0001_site", "qtl0001_study"), strPath = "inst/workflow/2_metrics", strPackage = "gsm.qtl")
reporting_wf <- gsm.core::MakeWorkflowList(strNames = c("Results", "Groups"), strPath = "workflow/3_reporting", strPackage = "gsm.reporting")

lRaw <- map_depth(ie_data, 1, gsm.mapping::Ingest, mappings_spec)
mapped <- map_depth(lRaw, 1, ~ gsm.core::RunWorkflows(mappings_wf, .x))
analyzed <- map_depth(mapped, 1, ~gsm.core::RunWorkflows(metrics_wf, .x))
reporting <- map2(mapped, analyzed, ~ gsm.core::RunWorkflows(reporting_wf, c(.x, list(lAnalyzed = .y, lWorkflows = metrics_wf))))

# Fix `SnapshotDate` column in reporting results, can be addressed in https://github.com/Gilead-BioStats/gsm.reporting/issues/24
dates <- names(ie_data) %>% as.Date
reporting <- map2(reporting, dates, ~{
  .x$Reporting_Results$SnapshotDate <- .y
  .x
})

# Bind multiple snapshots of data together
all_reportingResults <- do.call(dplyr::bind_rows, lapply(reporting, function(x) x$Reporting_Results))

# Only need 1 reporting group object
all_reportingGroups <- reporting[[6]]$Reporting_Groups

ie_listing <- analyzed[[length(analyzed)]]$Analysis_qtl0001_site$Analysis_Listing %>%
  filter(enrollyn == "N")

# Test if new Report_QTL rmd works
Report_QTL(
  dfResults = all_reportingResults,
  dfGroups = all_reportingGroups,
  dfListing = ie_listing,
  strOutputFile = "test.html"
)
