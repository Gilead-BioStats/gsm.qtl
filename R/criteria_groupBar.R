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
    ggplot(., aes(x = ietestcd_concat, fill = !!enexpr(varGroupID),
                  text = paste0(strGroupLabel,": ", !!enexpr(varGroupID),
                                "\nEligibility Status: ", ietestcd_concat))) +
    geom_bar() +
    labs(x = "Criteria", y = "Criteria Count", fill = strGroupLabel, title = paste0("Eligibility by ", strGroupLabel)) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # tilt to avoid overlap
      panel.grid.major.x = element_blank()
    )
  # Create plotly
  plotly::ggplotly(group_criteria_bar, tooltip = c("y", "text")) %>%
    layout(margin = list(l = 50, r = 50, b = 150, t = 50),
           annotations = list(x = 1, y = -0.5, text = "Note: Participants can be ineligible for multiple criteria.",
                              xref='paper', yref='paper', showarrow = F,
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font = list(size = 10)))
}
