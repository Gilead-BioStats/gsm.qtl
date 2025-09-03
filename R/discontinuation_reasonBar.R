#' Discontinuation Reasons Bar Chart
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#'
#' @returns A `plotly` object
#' @export
discontinuation_reasonBar <- function(df) {
  reasonbar <- df %>%
    # filter(compyn == "N") %>%
    ggplot(., aes(y = compreas, fill = compreas)) +
    geom_bar() +
    labs(y = "Discontinuation Reasons", x = "Participant Count", title = "Participant Count by Reasons") +
    theme_classic() +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )
  plotly::ggplotly(reasonbar, tooltip = c("x"))
}
