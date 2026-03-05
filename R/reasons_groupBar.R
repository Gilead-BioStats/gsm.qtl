#' Bar Chart by Group and Reasons
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid.
#' @param varCompreas A variable to identify study completion/discontinuation reasons
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#'
#' @returns A `plotly` object
#'
#' @export
reasons_groupBar <- function(df, varGroupID, varCompreas, strGroupLabel) {
  df_counts <- df %>%
    dplyr::count({{varCompreas}}, {{varGroupID}}, name = "n")

  distinct_n_compreas <- df_counts %>%
    dplyr::distinct({{varCompreas}}) %>%
    nrow()

  # Create GG object
  group_reasons_bar <- df_counts %>%
    # filter(compyn == "N") %>%
    ggplot(
      aes(
        x = n,
        y = {{varCompreas}},
        fill = {{varGroupID}},
        text = paste0(
          strGroupLabel, ": ", !!enexpr(varGroupID),
          "\n Discontinuation Reason: ", !!enexpr(varCompreas),
          "\nCount: ", n
        )
      )
    ) +
    geom_col() +
    geom_text(
      data = df_counts %>% group_by({{varCompreas}}) %>% summarise(n = sum(n), .groups = "drop"),
      aes(x = n, y = {{varCompreas}}, label = n),
      inherit.aes = FALSE,
      nudge_x = 0.5,
      size = 4,
      color = "black"
    ) +
    labs(y = "Reason", x = "Reason Count", fill = strGroupLabel, title = paste0("Discontinuation Reason by ", strGroupLabel)) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )
  # Create plotly
  plotly::ggplotly(group_reasons_bar, tooltip = c("text"), h = calc_fig_size(n_rows = distinct_n_compreas))
}
