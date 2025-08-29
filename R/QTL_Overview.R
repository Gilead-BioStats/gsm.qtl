#' QTL Overview Table
#'
#' @param dfResults A results `data.frame` from the output of `gsm.reporting::BindResults()` used
#' to create a variety of visualizations like the line plot, bar plot.
#' @param dSnapshot A `date` to determine the snapshot grab the data from `dfResults`
#' @param strNum A `string` to denominate what the numerator population is.
#' @param strDenom A `string` to denominate what the denominator population is.
#' @param strQTL A `string` to define the QTL being measured
#'
#' @returns `gt` object
#' @export

QTL_Overview <- function(dfResults,
                         dSnapshot,
                         strNum,
                         strDenom,
                         strQTL) {
  dfResults %>%
    filter(
      GroupLevel == "Study",
      SnapshotDate == dSnapshot,
      !(GroupID %in% c("Upper_funnel", "Flatline"))
    ) %>%
    mutate_at(c("Metric", "Upper_funnel", "Numerator", "Denominator", "Flag"), as.numeric) %>%
    mutate(
      qtlrate = paste0(as.character(round(Metric * 100, 3)), "%"),
      Deviation = ifelse(Metric > Upper_funnel, "Yes", "No"),
      Upper_funnel = paste0(as.character(round(Upper_funnel * 100, 3)), "%")
    ) %>%
    mutate_all(as.character) %>%
    select(GroupID, Numerator, Denominator, qtlrate, Upper_funnel, Deviation) %>%
    tidyr::pivot_longer(., cols = c(GroupID, Numerator, Denominator, qtlrate, Upper_funnel, Deviation), names_to = "Param", values_to = "Value") %>%
    mutate(Param = case_when(
      Param == "GroupID" ~ "StudyID",
      Param == "Numerator" ~ strNum,
      Param == "Denominator" ~ strDenom,
      Param == "qtlrate" ~ strQTL,
      Param == "Upper_funnel" ~ "Current Threshold",
      Param == "Deviation" ~ "Deviation?"
    )) %>%
    gt::gt()
}
