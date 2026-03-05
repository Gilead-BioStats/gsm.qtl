#' Bar Chart by Group and Criteria
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#'
#' @returns A `plotly` object
#'
#' @export
criteria_groupBar <- function(df, varGroupID, strGroupLabel) {
  var_sym  <- rlang::ensym(varGroupID)
  var_name <- rlang::as_string(var_sym)

  # Create GG object
  df_counts <- df %>%
    filter(!is.na(ietestcd_concat), nzchar(ietestcd_concat)) %>%
    tidyr::separate_longer_delim(ietestcd_concat, ",") %>%
    dplyr::count(ietestcd_concat, !!var_sym, name = "n")

  distinct_n_ie <- df_counts %>% dplyr::distinct(ietestcd_concat) %>% nrow()

  group_criteria_bar <- df_counts %>%
    ggplot(
      aes(
        x = n,
        y = ietestcd_concat,
        fill = .data[[var_name]],
        text = paste0(
          strGroupLabel, ": ", .data[[var_name]],
          "\nEligibility Status: ", ietestcd_concat,
          "\nCount: ", n
        )
      )
    ) +
    geom_col() +
    geom_text(
      data = df_counts %>% group_by(ietestcd_concat) %>% summarise(n = sum(n), .groups = "drop"),
      aes(x = n, y = ietestcd_concat, label = n),
      inherit.aes = FALSE,
      nudge_x = 0.1,
      size = 4,
      color = "black"
    ) +
    labs(y = "Criteria", x = "Criteria Count", fill = strGroupLabel, title = paste0("Eligibility by ", strGroupLabel)) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )

  # Create plotly
  plotly::ggplotly(group_criteria_bar, tooltip = c("text"), h = calc_fig_size(n_rows = distinct_n_ie))
}
