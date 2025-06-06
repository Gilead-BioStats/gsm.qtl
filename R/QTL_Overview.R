#' Title
#'
#' @param dfResults A results `data.frame` from the output of `gsm.reporting::BindResults()` used
#' to create a variety of visualizations like the line plot, bar plot.
#' @param strQTLid A `string` to denominate with QTL metric to be displayed
#'
#' @returns `gt` object
#' @export

QTL_Overview <- function(dfResults, strQTLid, dSnapshot) {
  dfResults %>%
    filter(GroupLevel == "Study",
           SnapshotDate == dSnapshot,
           !(GroupID %in% c("Upper_funnel", "Flatline")),
           MetricID == strQTLid) %>%
    mutate_at(c("Metric", "upper_funnel", "Numerator", "Denominator", "Flag"), as.numeric) %>%
    mutate(Ineligible_Rate = paste0(as.character(round(Metric,3)*100),"%"),
           Deviation = ifelse(Metric > upper_funnel, "Yes", "No"),
           upper_funnel = paste0(as.character(round(upper_funnel,3)*100), "%")) %>%
    mutate_all(as.character) %>%
    select(GroupID, Numerator, Denominator, Ineligible_Rate, upper_funnel, Deviation) %>%
    tidyr::pivot_longer(., cols = c(GroupID, Numerator, Denominator, Ineligible_Rate, upper_funnel, Deviation), names_to = "Param", values_to = "Value") %>%
    mutate(Param = case_when(
      Param == "GroupID" ~ "StudyID",
      Param == "Numerator" ~ "Ineligible",
      Param == "Denominator" ~ "Total Enrolled",
      Param == "Ineligible_Rate" ~ "Current Ineligible Enrolled Rate",
      Param == "upper_funnel" ~ "Current Threshold",
      Param == "Deviation" ~ "Deviation?"
    )) %>%
    gt::gt()
}
