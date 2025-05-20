#' Funnel Plot Analysis with Normal Approximation for Binary and Rate Outcomes.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Creates analysis results data for percentage/rate data using funnel plot
#' method with normal approximation.
#'
#'
#' @section Statistical Methods: This function applies funnel plots using
#'   a fixed proportion/rate and number of standard deviations according to the
#'   one-sided Z proportion test.
#'
#' @param dfTransformed `data.frame` Transformed data for analysis. Data should
#'   have one record per site with expected columns: `GroupID`, `GroupLevel`,
#'   `Numerator`, `Denominator`, and `Metric`. For more details see the Data
#'   Model vignette: `vignette("DataModel", package = "gsm.core")`. For this
#'   function, `dfTransformed` should typically be created using
#'   [Transform_Rate()].
#' @param nPropRate a numeric, between 0 and 1, that represents a proportion of
#' comparison, e.g. a historic screen failure rate
#' @param nNumDeviations a numeric, e.g. '3', standard deviations away from the value
#' provided in `nPropRate` to calculate a threshold to which the `Metric` should be flagged
#'
#' @return `data.frame` with one row per site with columns: GroupID, GroupLevel, Numerator,
#'   Denominator, Metric, upper_funnel and flag
#'
#' @examples
#' # Binary
#' dftransformed <- tibble::tribble(
#'   ~GroupID, ~GroupLevel, ~Numerator, ~Denominator, ~Metric,
#'   "ABC",     "Study",         25,          100,    0.25
#' )
#'
#' dfAnalyzed <- Analyze_OneSideProp(dftransformedd, nPropRate = 0.01, nNumDeviations = 3)
#'
#' @export

Analyze_OneSideProp <- function(
    dfTransformed,
    nPropRate = 0.1,
    nNumDeviations = 3
) {
  stop_if(cnd = !is.data.frame(dfTransformed), message = "dfTransformed is not a data.frame")
  stop_if(
    cnd = !all(c("GroupID", "GroupLevel", "Denominator", "Numerator", "Metric") %in% names(dfTransformed)),
    message = "One or more of these columns not found: GroupID, GroupLevel, Denominator, Numerator, Metric"
  )
  stop_if(cnd = !all(!is.na(dfTransformed[["GroupID"]])), message = "NA value(s) found in GroupID")
  stop_if(cnd = (nPropRate > 1 | nPropRate < 0), message = "`nPropRate` must be a value between 0 and 1.")

  # Calculate a grouped summarization that accounts for groups by study, site, country
  # This df should always be the same (i.e. equivalent) of study level
  Upper_funnel <- dfTransformed %>%
    group_by(GroupLevel) %>%
    summarize(GroupID = "Upper_funnel",
              Numerator = sum(Numerator),
              Denominator = sum(Denominator)) %>%
    ungroup() %>%
    mutate(Metric = nPropRate + nNumDeviations*sqrt(nPropRate*(1-nPropRate)/sum(.data$Denominator))) # To plot the funnel need funnel data under metric

  flat_line <- Upper_funnel %>%
    mutate(GroupID = "Flatline",
           Metric = nPropRate)

  # Bind the upper funnel back together with original dataframe
  dfScore <- dplyr::bind_rows(dfTransformed, Upper_funnel, flat_line) %>%
    mutate(
      vMu = nPropRate, # calculate one-sided proportion score against a historic rate
      z_0 = ifelse(.data$vMu == 0 | .data$vMu == 1,
                   0,
                   (.data$Metric - .data$vMu) /
                     sqrt(.data$vMu * (1 - .data$vMu) / .data$Denominator)
      ),
      # Unsure if a dispersion correction is necessary especially at a study-level?
      upper_funnel = nPropRate + nNumDeviations*sqrt(nPropRate*(1-nPropRate)/sum(.data$Denominator))
      # Additional cutoffs can be made to compare against `Metric` for flagging
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
      "Flag"
    )

  return(dfAnalyzed)
}
