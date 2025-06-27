#' Bar Chart by Group and Reasons
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#'
#' @returns A `plotly` object
#'
#' @export
reasons_groupBar <- function(df, varGroupID, strGroupLabel) {
  # Create GG object
  group_reasons_bar <- df %>%
    filter(compyn == "N") %>%
    ggplot(., aes(
      x = compreas, fill = !!enexpr(varGroupID),
      text = paste0(
        strGroupLabel, ": ", !!enexpr(varGroupID),
        "\n Discontinuation Reason: ", compreas
      )
    )) +
    geom_bar() +
    labs(x = "Reason", y = "Reason Count", fill = strGroupLabel, title = paste0("Discontinuation Reason by ", strGroupLabel)) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # tilt to avoid overlap
      panel.grid.major.x = element_blank()
    )
  # Create plotly
  plotly::ggplotly(group_reasons_bar, tooltip = c("y", "text")) %>%
    layout(
      margin = list(l = 50, r = 50, b = 150, t = 50),
      annotations = list(
        x = 1, y = -0.5,
        xref = "paper", yref = "paper", showarrow = F,
        xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
        font = list(size = 10)
      )
    )
}
