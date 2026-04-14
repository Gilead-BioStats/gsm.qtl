#' Eligibility Bar Chart by Source
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#'
#' @returns A `plotly` object
#' @export
eligibility_sourceBar <- function(df) {
  source_bar <- df %>%
    ggplot(
      aes(
        y = Source,
        fill = Source,
        text = paste0("\nSource: ", Source)
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
    labs(y = "Source", x = "Participant Count", title = "Participant Count by Category/Source") +
    theme_classic() +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )
  plotly::ggplotly(source_bar, tooltip = c("text"), h = calc_fig_size(n_rows = length(unique(df$Source)))) %>%
    layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
}
