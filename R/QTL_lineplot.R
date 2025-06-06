#' Title
#'
#' @param dfResults A results `data.frame` from the output of `gsm.reporting::BindResults()` used
#' to create a variety of visualizations like the line plot, bar plot.
#' @param strQTL A `string` to label the QTL being measured
#'
#' @returns A `plotly` object
#' @export
#'
#' @examples
QTL_lineplot <- function(dfResults,
                         strQTLid,
                         strQTL) {
  df_plot <- dfResults %>%
    filter(GroupLevel == "Study") %>%
    select(GroupID, Numerator, Denominator, Metric, SnapshotDate) %>%
    mutate(SnapshotDate = as.Date(SnapshotDate)) %>%
    # Add group type
    mutate(group_type = case_when(
      GroupID == "Upper_funnel" ~ "QTL Threshold",
      GroupID == "Flatline" ~ "Nominal Threshold",
      TRUE ~ strQTL
    ))



  # Pivot thresholds to wide format
  thresholds <- df_plot %>%
    filter(GroupID %in% c("Upper_funnel", "Flatline")) %>%
    select(SnapshotDate, GroupID, Metric) %>%
    tidyr::pivot_wider(names_from = GroupID, values_from = Metric)



  # Join thresholds to full data
  df_joined <- df_plot %>%
    left_join(thresholds, by = "SnapshotDate") %>%
    mutate(
      # Point color logic for main group only
      point_color = case_when(
        group_type != strQTL ~ NA_character_,
        Metric > `Upper_funnel` ~ "red",
        TRUE ~ "green"
      ),
      tooltip_text = case_when(
        group_type != strQTL ~ paste0(
          group_type, "
",
"Date: ", SnapshotDate, "
",
"Metric: ", round(Metric, 3)
        ),
TRUE ~ paste0(
  "Study: ", GroupID, "
",
"Date: ", SnapshotDate, "
",
"Metric: ", round(Metric, 3), "
",
"Numerator: ", Numerator, "
",
"Denominator: ", Denominator
)
      )
    )



  # Build ggplot
  p <- ggplot(
    df_joined,
    aes(x = SnapshotDate, y = Metric, group = GroupID, text = tooltip_text)
  ) +
    # Line with custom linetype
    geom_line(
      data = df_joined %>%
        filter(group_type %in% c(strQTL, "QTL Threshold", "Nominal Threshold")),
      aes(color = group_type, linetype = group_type, linewidth = group_type),
      size = 1,
      show.legend = FALSE
    ) +

    # Colored points for Ineligibility Rate (keep color, hide legend)
    geom_point(
      data = df_joined %>% filter(group_type == strQTL),
      aes(color = point_color),
      size = 2,
      show.legend = FALSE
    ) +

    # Points for thresholds (hide legend)
    geom_point(
      data = df_joined %>% filter(group_type != strQTL),
      aes(color = group_type),
      size = 1,
      show.legend = FALSE
    ) +

    # Combine all colors in scale but only show legend for line groups
    scale_color_manual(
      values = c(
        "QTL Threshold" = "#FF5859",
        "Nominal Threshold" = "grey80",
        strQTL = "grey50",
        "red" = "#FF5859",
        "green" = "#3DAF06"
      ),
      breaks = c(strQTL, "Nominal Threshold", "QTL Threshold"),
      name = NULL
    ) +
    scale_linetype_manual(
      values = c(
        "QTL Threshold" = "dashed",
        "Nominal Threshold" = "dotted",
        strQTL = "solid"
      )
    ) +
    scale_linewidth_manual(
      values = c(
        "QTL Threshold" = 0.75,
        "Nominal Threshold" = 0.5,
        strQTL = 0.75
      )
    ) +

    # scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30),
      legend.position = "none"
    ) +
    labs(
      y = strQTL,
      x = "Snapshot Date"
    )





  ggplotly(p, tooltip = "text")
}
