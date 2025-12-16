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
    mutate(Upper_funnel = .data$nPropRate + .data$nNumDeviations * sqrt(.data$nPropRate * (1 - .data$nPropRate) / .data$Denominator),
           Flatline = .data$nPropRate)

  Upper_funnel <- process_results %>%
    mutate(
      GroupID = "Upper_funnel",
      Metric = .data$Upper_funnel
    )

  flat_line <- Upper_funnel %>%
    mutate(
      GroupID = "Flatline",
      Metric = .data$nPropRate
    )

  binded_results <- dplyr::bind_rows(process_results, Upper_funnel, flat_line)
  return(binded_results)
}
