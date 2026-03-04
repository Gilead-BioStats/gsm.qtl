#' Discontinuation Reasons Bar Chart
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varCompreas A variable to identify study completion/discontinuation reasons

#' @returns A `plotly` object
#' @export
discontinuation_reasonBar <- function(df, varCompreas) {
  reasonbar <- df %>%
    ggplot(.,
      aes(y = {{varCompreas}},
          fill = {{varCompreas}},
          text = paste0(
            "Reason: ",!!enexpr(varCompreas),
            "<br>Count: ", after_stat(count)
          )
      )
    ) +
    geom_bar() +
    geom_text(
      stat = "count",
      aes(label = after_stat(count)),
      nudge_x = 1,
      color = "black",
      size  = 4
    ) +
    labs(y = "Discontinuation Reasons", x = "Participant Count", title = "Participant Count by Reasons") +
    theme_classic() +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )
  plotly::ggplotly(reasonbar, tooltip = c("text"), h = calc_fig_size(n_rows = dplyr::n_distinct(dplyr::pull(df, {{ varCompreas }}))))
}
