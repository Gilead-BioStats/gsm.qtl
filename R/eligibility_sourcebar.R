#' Title
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#'
#' @returns A `plotly` object
#' @export
eligibility_sourceBar <- function(df){
  source_bar <- df %>%
    filter(Source != "Neither") %>%
    ggplot(., aes(x = Source, fill = Source)) +
    geom_bar()+
    labs(x = "Source", y = "Participant Count", title = "Particpant Count by Category/Source") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # tilt to avoid overlap
      panel.grid.major.x = element_blank()
  )
  plotly::ggplotly(source_bar, tooltip = c("y", "x"))
}
