#' Eligibility Bar Chart by Source
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#'
#' @returns A `plotly` object
#' @export
eligibility_sourceBar <- function(df) {
  source_bar <- df %>%
    filter(Source != "Neither") %>%
    ggplot(., aes(y = Source, fill = Source)) +
    geom_bar() +
    labs(y = "Source", x = "Participant Count", title = "Participant Count by Category/Source") +
    theme_classic() +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )
  plotly::ggplotly(source_bar, tooltip = c("x"))
}
