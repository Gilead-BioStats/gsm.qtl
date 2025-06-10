#' Stacked Discontinuation Bar Chart
#'
#' @param df A `data.frame` containing the participant level dataset with discontinuation
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#'
#' @returns A `plotly` object
#'
#' @export
discontinuation_groupBar <- function(df, varGroupID, strGroupLabel) {
  # Parse out groups with 0 ineligible
  groups_with_discontinuation <- df %>%
    filter(compyn %in% c("N", "")) %>%
    pull(!!enexpr(varGroupID)) %>%
    unique()

  # Create the gg object
  group_bar <- df %>%
    mutate(fillcol = ifelse(compyn %in% c("N", ""), "Premature Discontinuation", "Completed/Ongoing")) %>%
    filter(!!enexpr(varGroupID) %in% groups_with_discontinuation) %>%
    mutate(!!enexpr(varGroupID) := forcats::fct_rev(forcats::fct_infreq(!!enexpr(varGroupID)))) %>%
    dplyr::group_by(!!enexpr(varGroupID), fillcol) %>%
    dplyr::summarize(totals = n()) %>%
    ungroup() %>%
    ggplot(., aes(y = !!enexpr(varGroupID),  fill = fillcol, x = totals,
                  text = paste0("Count: ", totals,
                                "\n", strGroupLabel ,": ", !!enexpr(varGroupID),
                                "\nDiscontinuation Status: ", fillcol))
    ) +
    geom_bar(stat = "identity") +
    labs(y = strGroupLabel, x = "Participant Count", fill = "Study Status", title = paste0("Participant Count by ", strGroupLabel)) +
    scale_fill_manual(values = c("Premature Discontinuation" = "#FF5859",
                                 "Completed/Ongoing" = "#00BFC4")) +
    theme_classic()

  # Create the plotly object
  x <- plotly::ggplotly(group_bar, tooltip = c("text")) %>%
    layout(margin = list(l = 50, r = 50, b = 150, t = 50),
           annotations = list(x = 1, y = -0.5, text = paste0("Note: Excludes ", tolower(strGroupLabel), "(s)", " with no ineligible participants."),
                              xref='paper', yref='paper', showarrow = F,
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font = list(size = 10)))
  return(x)
}
