#' Stacked Eligibility Bar Chart
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#' @param bPercentage A `boolean` to denote whether or not the group bar chart should be visualized as percentages instead of absolute counts.
#'
#' @returns A `plotly` object
#'
#' @export
eligibility_groupBar <- function(df, varGroupID, strGroupLabel, bPercentage = FALSE) {
  # Parse out groups with 0 ineligible
  groups_with_ineligible <- df %>%
    filter(Source != "Neither") %>%
    pull(!!enexpr(varGroupID)) %>%
    unique()

  # Create the gg object
  interim_bar <- df %>%
    mutate(fillcol = ifelse(Source == "Neither", "No Eligibility Risk", "Ineligible")) %>%
    filter(!!enexpr(varGroupID) %in% groups_with_ineligible) %>%
    mutate(!!enexpr(varGroupID) := forcats::fct_rev(forcats::fct_infreq(!!enexpr(varGroupID))),
           fillcol = fct_rev(fillcol)) %>%
    dplyr::group_by(!!enexpr(varGroupID), fillcol) %>%
    dplyr::summarize(totals = n(),
                     .groups = "keep") %>%
    ungroup() %>%
    dplyr::group_by(!!enexpr(varGroupID)) %>%
    dplyr::mutate(perc = round((100*totals/sum(totals)), 1)) %>%
    ungroup() %>%
    ggplot(., aes(
      y = !!enexpr(varGroupID), fill = fillcol, x = totals,
      text = paste0(
        "Count: ", totals,
        "\nPercentage: ", perc, " %",
        "\n", strGroupLabel, ": ", !!enexpr(varGroupID),
        "\nEligibility Status: ", fillcol
      )
    ))

  if(bPercentage) {
    group_bar <- interim_bar +
      geom_bar(stat = "identity", position = "fill") +
      labs(y = strGroupLabel, x = "Participant Percentage", fill = "Eligibility", title = paste0("Participant Percentage by ", strGroupLabel)) +
      scale_fill_manual(values = c(
        "Ineligible" = "#FF5859",
        "No Eligibility Risk" = "#00BFC4",
        "Neither" = "#7CAE00"
      )) +
      scale_x_continuous(labels = scales::percent_format()) +
      theme_classic()

  } else {
    group_bar <- interim_bar +
      geom_bar(stat = "identity") +
      labs(y = strGroupLabel, x = "Participant Count", fill = "Eligibility", title = paste0("Participant Count by ", strGroupLabel)) +
      scale_fill_manual(values = c(
        "Ineligible" = "#FF5859",
        "No Eligibility Risk" = "#00BFC4",
        "Neither" = "#7CAE00"
      )) +
      theme_classic()
  }

  # Create the plotly object
  x <- plotly::ggplotly(group_bar, tooltip = c("text"), h = calc_fig_size(n_row = length(groups_with_ineligible))) %>%
    layout(
      margin = list(l = 50, r = 50, b = 150, t = 50),
      annotations = list(
        x = 1, y = -0.5, text = paste0("Note: Excludes ", tolower(strGroupLabel), "(s)", " with no ineligible participants."),
        xref = "paper", yref = "paper", showarrow = F,
        xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
        font = list(size = 10)
      )
    )
  return(x)
}
