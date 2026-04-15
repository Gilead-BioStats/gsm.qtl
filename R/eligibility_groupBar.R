#' Stacked Eligibility Bar Chart
#'
#' @param dfNum A `data.frame` containing the participant level dataset with only ineligibility values
#' @param dfDenom A `data.frame` containing the participant level dataset with all inc/exc values.
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#' @param bPercentage A `boolean` to denote whether or not the group bar chart should be visualized as percentages instead of absolute counts.
#'
#' @returns A `plotly` object
#'
#' @export
eligibility_groupBar <- function(dfNum, dfDenom, varGroupID, strGroupLabel, bPercentage = FALSE) {
  # Parse out groups with 0 ineligible
  groups_with_ineligible <- dfNum %>%
    pull(!!enexpr(varGroupID)) %>%
    unique()

  # Create the gg object
  interim_bar <- dfDenom %>%
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
    ggplot(
     aes(
       y = !!enexpr(varGroupID),
       fill = fillcol,
       x = totals,
       text = paste0(
         "Count: ", totals,
         "\nPercentage: ", perc, " %",
         "\n", strGroupLabel, ": ", !!enexpr(varGroupID),
         "\nEligibility Status: ", fillcol
       )
      )
    )

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

  n_groups_without_ineligible <- dfDenom %>%
    filter(!(!!enexpr(varGroupID) %in% groups_with_ineligible)) %>%
    pull(!!enexpr(varGroupID)) %>%
    unique() %>%
    length()

  n_participants_without_ineligible <- dfDenom %>%
    filter(!(!!enexpr(varGroupID) %in% groups_with_ineligible)) %>%
    nrow()

   # Add footnote text to plot if there are groups with 0 ineligible participants
  footnote_text <- if (n_groups_without_ineligible > 0) {
    paste0("Note: Excludes ", n_groups_without_ineligible, " ", tolower(strGroupLabel), "(s) with no ineligible participants (", n_participants_without_ineligible, " participants).")
  } else {
    NULL
  }

  footnote_layout <- calc_plotly_footnote_layout(footnote_text)

  # Create the plotly object
  x <- plotly::ggplotly(group_bar, tooltip = c("text"), h = calc_fig_size(n_rows = length(groups_with_ineligible))) %>%
    layout(
      margin = footnote_layout$margin,
      annotations = footnote_layout$annotations,
      xaxis = list(autorange = TRUE),
      yaxis = list(autorange = TRUE)
    )
  return(x)
}
