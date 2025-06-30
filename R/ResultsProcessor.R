#' Title
#'
#' @param df a data.frame
#'
#' @export
ResultsProcessor <- function(df) {
  df1 <- df

  df2 <- df1 %>%
    filter(
      str_detect(MetricID, "qtl"),
      GroupID %in% c("Upper_funnel", "Flatline")
    ) %>%
    select(SnapshotDate, MetricID, GroupID, Metric) %>%
    mutate(GroupID = tolower(GroupID)) %>%
    tidyr::pivot_wider(., names_from = "GroupID", values_from = "Metric")

  dfResults <- full_join(df1, df2, by = c("SnapshotDate", "MetricID"))
  return(dfResults)
}
