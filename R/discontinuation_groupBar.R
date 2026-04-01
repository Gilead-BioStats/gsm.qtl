#' Stacked Discontinuation Bar Chart
#'
#' @param df A `data.frame` containing the participant level dataset with discontinuation
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#' @param varStatus A variable indicating participant study status, defaults to `compyn`.
#' @param valuesDiscontinued A vector of values in `varStatus` considered premature discontinuations, defaults to 'N'.
#'
#' @returns A `plotly` object
#'
#' @export
discontinuation_groupBar <- function(df,
                                     varGroupID,
                                     strGroupLabel,
                                     varStatus = compyn,
                                     valuesDiscontinued = c('N')) {
  groups_with_discontinuation <- df %>%
    filter(!!enexpr(varStatus) %in% valuesDiscontinued) %>%
    pull(!!enexpr(varGroupID)) %>%
    unique()

  # Create the gg object
  group_bar <- df %>%
    mutate(fillcol = ifelse(!!enexpr(varStatus) %in% valuesDiscontinued, "Premature Discontinuation", "Completed/Ongoing")) %>%
    filter(!!enexpr(varGroupID) %in% groups_with_discontinuation) %>%
    mutate(!!enexpr(varGroupID) := forcats::fct_rev(forcats::fct_infreq(!!enexpr(varGroupID)))) %>%
    dplyr::group_by(!!enexpr(varGroupID), fillcol) %>%
    dplyr::summarize(totals = n()) %>%
    ungroup() %>%
    ggplot(., aes(
      y = !!enexpr(varGroupID), fill = fillcol, x = totals,
      text = paste0(
        "Count: ", totals,
        "\n", strGroupLabel, ": ", !!enexpr(varGroupID),
        "\nDiscontinuation Status: ", fillcol
      )
    )) +
    geom_bar(stat = "identity") +
    labs(y = strGroupLabel, x = "Participant Count", fill = "Study Status", title = paste0("Participant Count by ", strGroupLabel)) +
    scale_fill_manual(values = c(
      "Premature Discontinuation" = "#FF5859",
      "Completed/Ongoing" = "#00BFC4"
    )) +
    theme_classic()

  n_groups_without_discontinuation <- df %>%
    filter(!(!!enexpr(varGroupID) %in% groups_with_discontinuation)) %>%
    pull(!!enexpr(varGroupID)) %>%
    unique() %>%
    length()

  n_participants_without_discontinuation <- df %>%
    filter(!(!!enexpr(varGroupID) %in% groups_with_discontinuation)) %>%
    nrow()

  footnote_text <- if (n_groups_without_discontinuation > 0) {
    paste0(
      "Note: Excludes ",
      n_groups_without_discontinuation,
      " ",
      tolower(strGroupLabel),
      "(s) with no prematurely discontinued participants (",
      n_participants_without_discontinuation,
      " participants)."
    )
  } else {
    NULL
  }

  footnote_layout <- calc_plotly_footnote_layout(footnote_text)

  # Create the plotly object
  x <- plotly::ggplotly(group_bar, tooltip = c("text"), h = calc_fig_size(n_rows = length(groups_with_discontinuation))) %>%
    layout(
      margin = footnote_layout$margin,
      annotations = footnote_layout$annotations
    )
  return(x)
}
