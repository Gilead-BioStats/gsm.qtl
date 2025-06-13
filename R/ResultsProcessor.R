#' Title
#'
#' @param df a data.frame
#'
#' @export
ResultsProcessor <- function(df){
  df <- params$dfResults

  df2 <- df %>%
    filter(str_detect(MetricID, "qtl"),
           GroupID %in% c("Upper_funnel", "Flatline")) %>%
    select(SnapshotDate, GroupID, Metric) %>%
    mutate(GroupID = tolower(GroupID)) %>%
    tidyr::pivot_wider(., names_from = "GroupID", values_from = "Metric")

  dfResults <- full_join(df, df2, "SnapshotDate")
  return(dfResults)
}
