#' Title
#'
#' @param dfResults a reporting results data.frame
#' @param dfMetrics a reporting metrics data.frame
#'
#' @export
ResultsProcessor <- function(dfResults, dfMetrics) {
  process_metrics <- dfMetrics %>%
    select(MetricID, nPropRate, nNumDeviations)

  process_results <- dfResults %>%
    left_join(., process_metrics, by = "MetricID") %>%
    mutate(Upper_funnel = nPropRate + nNumDeviations * sqrt(nPropRate * (1 - nPropRate) / Denominator),
           Flatline = nPropRate)

  Upper_funnel <- process_results %>%
    mutate(
      GroupID = "Upper_funnel",
      Metric = Upper_funnel
    )

  flat_line <- Upper_funnel %>%
    mutate(
      GroupID = "Flatline",
      Metric = nPropRate
    )

  binded_results <- dplyr::bind_rows(process_results, Upper_funnel, flat_line)
  return(binded_results)
}
