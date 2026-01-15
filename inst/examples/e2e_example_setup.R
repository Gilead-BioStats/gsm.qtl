library(gsm.template) # qtl_tabletop
library(gsm.datasim) # qtl_tabletop 
library(gsm.core) #qtl_tabletop
library(gsm.mapping) # fix-91
library(gsm.kri)
library(gsm.reporting)
library(gsm.qtl)
library(grail) # fix-379
library(grail.ado) # qtl_tabletop
library(gsm.rrm)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(fs)

# Run once
gsm.core::RunWorkflow(
  lWorkflow = yaml::read_yaml(
    gsm.template::GetPackageFile(
      'initialize-study.yaml'
    )
  ),
  lData = list(
    strStudyID = 'test risk signal process' # Replace with the actual study ID.
  )
)

setwd("./test risk signal process")


# ----
# Simulate study data and run end-to-end snapshots.
core_mappings <- c("AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB", "PD", "PK", "QUERY", "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ", "VISIT")
qtl_mappings <- c("IE", "EXCLUSION")

raw_data <- gsm.datasim::generate_rawdata_for_single_study(
  SnapshotCount = 4,
  SnapshotWidth = "3 months", 
  ParticipantCount = 100, 
  SiteCount = 10, 
  StudyID = "test risk signal process",
  workflow_path = "workflow/1_mappings",
  strStartDate = "1991-01-01",
  mappings = c(core_mappings, qtl_mappings),
  package = "gsm.mapping"
)


purrr::iwalk(raw_data, function(domains, date){
  dir <- path("data", date, "Raw")
  dir_create(dir)
  
  purrr::iwalk(domains, function(domain, domain_name){
    arrow::write_parquet(domain, path(dir, paste0(domain_name, ".parquet")))
  })
})

snapshot_dates <- names(raw_data)

for(i in 1:length(snapshot_dates)){
  lConfig <-  gsm.template::UpdateConfig(
    strStudyID = "test risk signal process",
    strSnapshotDate = snapshot_dates[i]
  )
  
  mapped_data <- './workflows/1_mappings' %>%
    gsm.core::MakeWorkflowList(
      strNames =c(core_mappings, qtl_mappings),
      strPath = .,
      strPackage = NULL
    ) %>%
    gsm.core::RunWorkflows(lConfig = lConfig)
  
  analysis_data <- './workflows/2_metrics' %>%
    gsm.core::MakeWorkflowList(
      c("qtl"),
      strPath = .,
      strPackage = NULL
    ) %>%
    gsm.core::RunWorkflows(lConfig = lConfig)
  
  reporting_data <- './workflows/3_reporting' %>%
    gsm.core::MakeWorkflowList(
      c("Groups", "Metrics", "Bounds", "Results", "SnapshotWorkItem", "LinkQTLWorkItems" , "RiskSignals_QTL", "QTLWorkItems"),  
      strPath = .,
      strPackage = NULL
    ) %>% 
    gsm.core::RunWorkflows(lConfig = lConfig)
  
  module_outputs <- './workflows/4_modules' %>%
    gsm.core::MakeWorkflowList(c("report_qtl"), strPath = ., strPackage = NULL) %>%
    gsm.core::RunWorkflows(lConfig = lConfig)
  
}

end_study_wf <- gsm.core::MakeWorkflowList(strNames = c("EndOfStudyQTLs"), strPath = "workflows")
gsm.core::RunWorkflows(end_study_wf, lConfig = lConfig)
