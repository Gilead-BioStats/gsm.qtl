#' Bar Chart by Group and Criteria
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#'
#' @returns A `plotly` object
#'
#' @export
criteria_groupBar <- function(df, varGroupID, strGroupLabel) {
  # Create GG object
  group_criteria_bar <- df %>%
    filter(!is.na(ietestcd_concat) | Source == "Eligibility IPD") %>%
    tidyr::separate_longer_delim(ietestcd_concat, ";;;") %>%
    mutate(ietestcd_concat = ifelse(Source == "Eligibility IPD", "No EDC I/E available", ietestcd_concat)) %>%
    ggplot(., aes(
      y = ietestcd_concat, fill = !!enexpr(varGroupID),
      text = paste0(
        strGroupLabel, ": ", !!enexpr(varGroupID),
        "\nEligibility Status: ", ietestcd_concat
      )
    )) +
    geom_bar() +
    labs(y = "Criteria", x = "Criteria Count", fill = strGroupLabel, title = paste0("Eligibility by ", strGroupLabel)) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )

  distinct_n_ie <-  df %>%
    filter(!is.na(ietestcd_concat) | Source == "Eligibility IPD") %>%
    tidyr::separate_longer_delim(ietestcd_concat, ";;;") %>%
    mutate(ietestcd_concat = ifelse(Source == "Eligibility IPD", "No EDC I/E available", ietestcd_concat)) %>%
    pull(ietestcd_concat) %>%
    unique() %>%
    length()

  # Create plotly
  plotly::ggplotly(group_criteria_bar, tooltip = c("y", "text"), h = calc_fig_size(n_rows = distinct_n_ie, per = 35))
}
