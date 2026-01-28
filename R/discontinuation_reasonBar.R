#' Discontinuation Reasons Bar Chart
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varCompreas A variable to identify study completion/discontinuation reasons

#' @returns A `plotly` object
#' @export
discontinuation_reasonBar <- function(df, varCompreas) {
  reasonbar <- df %>%
    filter({{varCompreas}} != "") %>%
    ggplot(., aes(y = {{varCompreas}}, fill = {{varCompreas}})) +
    geom_bar() +
    geom_text(
      stat = "count",
      aes(label = after_stat(count)),
      hjust = - 1.5,
      color = "black",
      size  = 3
    ) +
    labs(y = "Discontinuation Reasons", x = "Participant Count", title = "Participant Count by Reasons") +
    theme_classic() +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )
  plotly::ggplotly(reasonbar, tooltip = c("x"), h = calc_fig_size(n_rows = dplyr::n_distinct(dplyr::pull(df, {{ varCompreas }}))))
}
