#' QTL Time Series Widget (v2)
#'
#' @param dfResults A results `data.frame` from the output of `gsm.reporting::BindResults()` used
#' to create time-series visualizations.
#' @param strQTL A `string` to label the QTL being measured.
#'
#' @returns A `htmlwidget` object.
#' @export
QTL_lineplot_v2 <- function(dfResults, strQTL) {
  gsm.core::stop_if(cnd = !is.data.frame(dfResults), "dfResults is not a data.frame")
  gsm.core::stop_if(cnd = !is.character(strQTL) || length(strQTL) != 1, "strQTL must be a length-1 character")

  threshold_aliases <- c("upper_funnel", "flatline", "flat_line")

  get_col_value <- function(df, candidate_names) {
    df_names <- names(df)
    # Find the first candidate that matches a column name in df, case-insensitively
    idx <- match(tolower(candidate_names), tolower(df_names), nomatch = 0)
    idx <- idx[idx > 0]
    if (length(idx) == 0) {
      return(rep(NA_real_, nrow(df)))
    }

    as.numeric(df[[df_names[idx[1]]]])
  }

  if (!"GroupID" %in% names(dfResults)) {
    stop("dfResults must contain GroupID")
  }

  if ("GroupLevel" %in% names(dfResults)) {
    df_plot <- dfResults %>%
      filter(tolower(.data$GroupLevel) == "study")
  } else {
    df_plot <- dfResults
  }

  df_plot <- df_plot %>%
    mutate(
      GroupID = as.character(.data$GroupID),
      GroupID_norm = tolower(.data$GroupID),
      SnapshotDate = as.Date(.data$SnapshotDate),
      Metric = as.numeric(.data$Metric),
      Numerator = as.numeric(.data$Numerator),
      Denominator = as.numeric(.data$Denominator)
    )

  df_main <- df_plot %>%
    filter(!(.data$GroupID_norm %in% threshold_aliases))

  upper_vals <- get_col_value(df_main, c("Upper_funnel", "upper_funnel"))
  flat_vals <- get_col_value(df_main, c("flat_line", "Flatline", "flatline"))

  threshold_rows <- df_main %>%
    mutate(
      Upper_funnel_plot = upper_vals,
      flat_line_plot = flat_vals
    )

  threshold_upper <- threshold_rows %>%
    mutate(
      GroupID = "Upper_funnel",
      GroupID_norm = "upper_funnel",
      Metric = .data$Upper_funnel_plot,
      Flag = NA_character_
    )

  threshold_flat <- threshold_rows %>%
    mutate(
      GroupID = "flat_line",
      GroupID_norm = "flat_line",
      Metric = .data$flat_line_plot,
      Flag = NA_character_
    )

  legacy_thresholds <- df_plot %>%
    filter(.data$GroupID_norm %in% threshold_aliases) %>%
    mutate(
      GroupID = dplyr::case_when(
        .data$GroupID_norm == "upper_funnel" ~ "Upper_funnel",
        TRUE ~ "flat_line"
      ),
      Flag = NA_character_
    )

  if (all(is.na(threshold_upper$Metric)) && any(legacy_thresholds$GroupID == "Upper_funnel")) {
    threshold_upper <- legacy_thresholds %>%
      filter(.data$GroupID == "Upper_funnel")
  }

  if (all(is.na(threshold_flat$Metric)) && any(legacy_thresholds$GroupID == "flat_line")) {
    threshold_flat <- legacy_thresholds %>%
      filter(.data$GroupID == "flat_line")
  }

  threshold_upper <- threshold_upper %>%
    filter(!is.na(.data$Metric))

  threshold_flat <- threshold_flat %>%
    filter(!is.na(.data$Metric))

  upper_lookup <- threshold_upper %>%
    select(.data$SnapshotDate, Upper_funnel_lookup = .data$Metric) %>%
    distinct(.data$SnapshotDate, .keep_all = TRUE)

  df_main <- df_main %>%
    left_join(upper_lookup, by = "SnapshotDate") %>%
    mutate(
      Upper_funnel_plot = dplyr::coalesce(
        as.numeric(get_col_value(., c("Upper_funnel", "upper_funnel"))),
        as.numeric(.data$Upper_funnel_lookup)
      )
    ) %>%
    mutate(
      Flag = dplyr::case_when(
        !is.na(.data$Upper_funnel_plot) & .data$Metric > .data$Upper_funnel_plot ~ "Above QTL Threshold",
        TRUE ~ "Below QTL Threshold"
      )
    )

  df_widget <- bind_rows(df_main, threshold_upper, threshold_flat) %>%
    mutate(GroupLevel = "Study") %>%
    arrange(.data$SnapshotDate)

  selected_groups <- df_main %>%
    distinct(.data$GroupID) %>%
    pull(.data$GroupID)

  lMetric <- list(
    MetricID = if ("MetricID" %in% names(df_widget)) unique(df_widget$MetricID)[1] else "QTL",
    Abbreviation = strQTL,
    GroupLevel = "Study",
    selectedGroupIDs = selected_groups
  )

  Widget_TimeSeriesQTL(
    dfResults = df_widget,
    lMetric = lMetric,
    dfGroups = NULL,
    strOutcome = "Metric",
    bAddGroupSelect = TRUE,
    bDebug = FALSE
  )
}
