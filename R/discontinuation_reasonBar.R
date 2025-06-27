#' Discontinuation Reasons Bar Chart
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#'
#' @returns A `plotly` object
#' @export
discontinuation_reasonBar <- function(df) {
  reasonbar <- df %>%
    filter(compyn == "N") %>%
    ggplot(., aes(x = compreas, fill = compreas)) +
    geom_bar() +
    labs(x = "Discontinuation Reasons", y = "Participant Count", title = "Participant Count by Reasons") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # tilt to avoid overlap
      panel.grid.major.x = element_blank()
    )
  plotly::ggplotly(reasonbar, tooltip = c("y", "x"))
}
