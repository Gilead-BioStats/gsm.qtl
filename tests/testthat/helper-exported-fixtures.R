qtl_test_participant_df <- function() {
  tibble::tribble(
    ~invid, ~country, ~subjid, ~Source, ~ietestcd_concat, ~dvdtm, ~eligibility_criteria, ~compyn, ~compreas,
    "S01", "US", "SUBJ-001", "EDC", "I001;;;E010", "2024-01-01;;;2024-01-03", "Criterion A;;;Criterion B", "N", "Adverse event",
    "S01", "US", "SUBJ-002", "Eligibility IPD only", "", "2024-01-02", "Criterion C", "", "Lost to follow-up",
    "S01", "US", "SUBJ-003", "Neither", "", "2024-01-04", "", "Y", "",
    "S02", "CA", "SUBJ-004", "EDC", "I002", "2024-02-01", "Criterion D", "N", "Protocol deviation",
    "S02", "CA", "SUBJ-005", "Neither", "", "2024-02-03", "", "Y", "",
    "S03", "GB", "SUBJ-006", "EDC", "I003;;;E020", "2024-03-01;;;2024-03-09", "Criterion E;;;Criterion F", "", "Withdrawal"
  )
}

qtl_test_results_df <- function() {
  tibble::tribble(
    ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric, ~SnapshotDate, ~Upper_funnel, ~Flag,
    "StudyA", "Study", 10, 100, 0.10, as.Date("2024-01-01"), 0.12, 0,
    "Upper_funnel", "Study", NA, NA, 0.12, as.Date("2024-01-01"), NA, NA,
    "Flatline", "Study", NA, NA, 0.05, as.Date("2024-01-01"), NA, NA,
    "StudyA", "Study", 16, 100, 0.16, as.Date("2024-02-01"), 0.13, 2,
    "Upper_funnel", "Study", NA, NA, 0.13, as.Date("2024-02-01"), NA, NA,
    "Flatline", "Study", NA, NA, 0.05, as.Date("2024-02-01"), NA, NA
  )
}

qtl_test_processor_inputs <- function() {
  df_results <- tibble::tribble(
    ~MetricID, ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric, ~SnapshotDate,
    "qtl0001", "StudyA", "Study", 10, 100, 0.10, as.Date("2024-01-01"),
    "qtl0001", "StudyB", "Study", 20, 120, 0.1667, as.Date("2024-01-01")
  )

  df_metrics <- tibble::tribble(
    ~MetricID, ~nPropRate, ~nNumDeviations,
    "qtl0001", 0.05, 3
  )

  list(dfResults = df_results, dfMetrics = df_metrics)
}