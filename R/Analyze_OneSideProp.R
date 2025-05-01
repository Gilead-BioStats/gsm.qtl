#' Funnel Plot Analysis with Normal Approximation for Binary and Rate Outcomes.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Creates analysis results data for percentage/rate data using funnel plot
#' method with normal approximation.
#'
#' More information can be found in [The Normal Approximation
#' Method](https://gilead-biostats.github.io/gsm.core/articles/KRI%20Method.html#the-normal-approximation-method)
#' of the KRI Method vignette.
#'
#' @section Statistical Methods: This function applies funnel plots using
#'   asymptotic limits based on the normal approximation of a binomial
#'   distribution for the binary outcome, or normal approximation of a Poisson
#'   distribution for the rate outcome with volume (the sample sizes or total
#'   exposure of the sites) to assess data quality and safety.
#'
#' @param dfTransformed `data.frame` Transformed data for analysis. Data should
#'   have one record per site with expected columns: `GroupID`, `GroupLevel`,
#'   `Numerator`, `Denominator`, and `Metric`. For more details see the Data
#'   Model vignette: `vignette("DataModel", package = "gsm.core")`. For this
#'   function, `dfTransformed` should typically be created using
#'   [Transform_Rate()].
#' @param proprate a numeric with suggested discontinuation rate
#'
#' @return `data.frame` with one row per site with columns: GroupID, Numerator,
#'   Denominator, Metric, OverallMetric, Factor, and Score.
#'
#' @examples
#' # Binary
#' dfTransformed <- Transform_Rate(analyticsInput)
#'
#' dfAnalyzed <- Analyze_NormalApprox(dfTransformed, strType = "binary")
#'
#' # Rate
#' dfAnalyzed <- Analyze_NormalApprox(dfTransformed, strType = "rate")
#'
#' @export

Analyze_OneSideProp <- function(
    dfTransformed,
    proprate = 0.1,
    num_deviations = 3
) {
  stop_if(cnd = !is.data.frame(dfTransformed), message = "dfTransformed is not a data.frame")
  stop_if(
    cnd = !all(c("GroupID", "GroupLevel", "Denominator", "Numerator", "Metric") %in% names(dfTransformed)),
    message = "One or more of these columns not found: GroupID, GroupLevel, Denominator, Numerator, Metric"
  )
  stop_if(cnd = !all(!is.na(dfTransformed[["GroupID"]])), message = "NA value(s) found in GroupID")

  # Calculate a grouped summarization to bind back together to create appropriate flags
  # groups by study, site, country this df should always be the same, i.e. equivalent of study level
  Upper_funnel <- dfTransformed %>%
    group_by(GroupLevel) %>%
    summarize(GroupID = "Upper_funnel",
              Numerator = sum(Numerator),
              Denominator = sum(Denominator)) %>%
    ungroup() %>%
    mutate(Metric = proprate + num_deviations*sqrt(proprate*(1-proprate)/sum(.data$Denominator)))

  dfScore <- dplyr::bind_rows(dfTransformed, Upper_funnel) %>%
    mutate(
      vMu = proprate,
      z_0 = ifelse(.data$vMu == 0 | .data$vMu == 1,
                   0,
                   (.data$Metric - .data$vMu) /
                     sqrt(.data$vMu * (1 - .data$vMu) / .data$Denominator)
      ),
      upper_funnel = proprate + num_deviations*sqrt(proprate*(1-proprate)/sum(.data$Denominator)) # can add additional cutoffs with additional function arguments
    ) %>%
    mutate(Flag = ifelse(Metric >= upper_funnel | GroupID == "Upper_funnel", 2, 0)) # Custom flag based on upper-funnel

  # dfAnalyzed -----------------------------------------------------------------
  dfAnalyzed <- dfScore %>%
    select(
      "GroupID",
      "GroupLevel",
      "Numerator",
      "Denominator",
      "Metric",
      Score = "z_0",
      Flag
    )


  LogMessage(
    level = "info",
    message = "`OverallMetric`, `Factor`, and `Score` columns created from normal approximation.",
    cli_detail = "inform"
  )

  return(dfAnalyzed)
}
