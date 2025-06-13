library(gsm.datasim)
library(gsm.mapping)
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
  workflow_path = "workflow/1_mappings",
  mappings = c("IE", "PD", "STUDCOMP"),
  package = "gsm.mapping",
  desired_specs = NULL
)

mappings_wf <- gsm.core::MakeWorkflowList(
  strNames =c("SUBJ", "ENROLL", "IE", "PD", "STUDY", "SITE", "COUNTRY", "EXCLUSION", "STUDCOMP"),
  strPath = "workflow/1_mappings",
  strPackage = "gsm.mapping"
)
mappings_spec <- gsm.mapping::CombineSpecs(mappings_wf)
metrics_wf <- gsm.core::MakeWorkflowList(strNames = c("qtl0001_study", "qtl0002_study"), strPath = "inst/workflow/2_metrics", strPackage = "gsm.qtl")
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
all_reportingResults <- do.call(dplyr::bind_rows, lapply(reporting, function(x) x$Reporting_Results)) %>%
  filter(MetricID == "Analysis_qtl0001_study") %>%
  select(-c(upper_funnel, flatline))

# Only need 1 reporting group object
all_reportingGroups <- reporting[[length(reporting)]]$Reporting_Groups

report_listings <- list(qtl0001 = mapped$`2012-06-30`$Mapped_EXCLUSION,
                        qtl0002 = left_join(mapped$`2012-06-30`$Mapped_STUDCOMP,
                                            select(mapped$`2012-06-30`$Mapped_SUBJ, subjid, invid, country),
                                            by = "subjid"))

# Test if new Report_QTL rmd works
Report_QTL(
  dfResults = all_reportingResults,
  dfGroups = all_reportingGroups,
  lListings = report_listings,
  strOutputFile = "test.html"
)
