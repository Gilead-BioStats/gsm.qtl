library(gsm.datasim) # requires >= fix-50
library(gsm.mapping) # requires >= fix-58
library(gsm.reporting)
library(gsm.core)
library(purrr)
library(dplyr)
devtools::load_all()
set.seed(1234)

# Single Study
ie_data <- generate_rawdata_for_single_study(
  SnapshotCount = 3,
  SnapshotWidth = "months",
  ParticipantCount = 100,
  SiteCount = 3,
  StudyID = "ABC",
  workflow_path = "workflow",
  mappings = "IE",
  package = "gsm.mapping",
  desired_specs = NULL
)

mappings_wf <- gsm.core::MakeWorkflowList(strNames =c("SUBJ", "ENROLL", "IE", "STUDY", "SITE", "COUNTRY"), strPath = "workflow/1_mappings", strPackage = "gsm.mapping")
mappings_spec <- gsm.mapping::CombineSpecs(mappings_wf)
metrics_wf <- gsm.core::MakeWorkflowList(strNames = "qtl0001_study", strPath = "inst/workflow/2_metrics", strPackage = "gsm.qtl")
reporting_wf <- gsm.core::MakeWorkflowList(strPath = "workflow/3_reporting", strPackage = "gsm.reporting")


lRaw2 <- map_depth(ie_data, 1, gsm.mapping::Ingest, mappings_spec)
mapped <- map_depth(lRaw, 1, ~ gsm.core::RunWorkflows(mappings_wf, .x))
analyzed <- map_depth(mapped, 1, ~gsm.core::RunWorkflows(metrics_wf, .x))
reporting <- map2(
  mapped, analyzed,
  ~ gsm.core::RunWorkflows(reporting_wf, c(.x, list(lAnalyzed = .y, lWorkflows = metrics_wf)))
)
dates <- names(ie_data) %>% as.Date
for(snap in seq_along(ie_data)){
    reporting[[snap]]$Reporting_Results$SnapshotDate = dates[snap]
    reporting[[snap]]$Reporting_Bounds$SnapshotDate = dates[snap]
}

all_reportingResults <- do.call(dplyr::bind_rows, lapply(reporting, function(x) x$Reporting_Results))
all_reportingGroups <- reporting[[snap]]$Reporting_Groups
all_reportingBounds <- do.call(dplyr::bind_rows, lapply(reporting, function(x) x$Reporting_Bounds))
all_reportingMetrics <- reporting[[snap]]$Reporting_Metrics

qtl_chart <- gsm.kri::Widget_TimeSeries(
  dfResults = all_reportingResults,
  dfGroups = all_reportingGroups,
  strOutcome = "Metric"
)

# Multiple Studies
# ie_data2 <- raw_data_generator(template_path = "~/gsm.datasim/inst/small_template.csv", mappings = "IE", package = "gsm.mapping")
# lRaw2 <- map_depth(ie_data2, 2, gsm.mapping::Ingest, mappings_spec)
# mapped2 <- map_depth(lRaw2, 2, ~ gsm.core::RunWorkflows(mappings_wf, .x))
# analyzed2 <- map_depth(mapped2, 2, ~gsm.core::RunWorkflows(metrics_wf, .x))
# reporting2 <- map2(
#   mapped2, analyzed2,
#   ~ pmap(list(.x, .y),
#          ~ gsm.core::RunWorkflows(reporting_wf, c(..1, list(lAnalyzed = ..2, lWorkflows = metrics_wf))))
# )
#
# reporting2 <- map(
#   reporting2,
#   ~ imap(
#     .x,                            # each “date” sub-list
#     function(record, date_string) {
#       # parse the name into an actual Date
#       d <- as.Date(date_string)
#
#       # overwrite both snapshots
#       record$Reporting_Results$SnapshotDate <- d
#       record$Reporting_Bounds$SnapshotDate  <- d
#
#       record
#     }
#   )
# )
#
# all_reportingResults2 <- map_dfr(reporting2,~ map_dfr(.x, "Reporting_Results"))
# all_reportingGroups2 <- map_dfr(reporting2, ~ map_dfr(.x, "Reporting_Groups")) %>% unique
# all_reportingBounds2 <- map_dfr(reporting2,~ map_dfr(.x, "Reporting_Bounds"))
# all_reportingMetrics2 <- map_dfr(reporting2,~ map_dfr(.x, "Reporting_Metrics")) %>% unique
#
# qtl_chart2 <- gsm.kri::Widget_TimeSeries(
#   dfResults = all_reportingResults2,
#   dfGroups = all_reportingGroups2,
#   strOutcome = "Metric"
# )
