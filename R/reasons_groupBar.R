#' Bar Chart by Group and Reasons
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid.
#' @param varCompreas A variable to identify study completion/discontinuation reasons
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#' @param bSwapAxes A `boolean` to denote whether or not the y-axis and fill groups should be swapped.
#'
#' @returns A `plotly` object
#'
#' @export
reasons_groupBar <- function(df, varGroupID, varCompreas, strGroupLabel, bSwapAxes = FALSE) {
  var_sym <- rlang::ensym(varGroupID)
  var_name <- rlang::as_string(var_sym)
  compreas_sym <- rlang::ensym(varCompreas)
  compreas_name <- rlang::as_string(compreas_sym)

  df_counts <- df %>%
    dplyr::count(.data[[compreas_name]], .data[[var_name]], name = "n")

  if (bSwapAxes) {
    distinct_n_y <- df_counts %>% dplyr::distinct(.data[[var_name]]) %>% nrow()

    group_reasons_bar <- df_counts %>%
      ggplot(
        aes(
          x = n,
          y = .data[[var_name]],
          fill = .data[[compreas_name]],
          text = paste0(
            strGroupLabel, ": ", .data[[var_name]],
            "\nDiscontinuation Reason: ", .data[[compreas_name]],
            "\nCount: ", n
          )
        )
      ) +
      geom_col() +
      geom_text(
        data = df_counts %>% dplyr::group_by(.data[[var_name]]) %>% dplyr::summarise(n = sum(n), .groups = "drop"),
        aes(x = n, y = .data[[var_name]], label = n),
        inherit.aes = FALSE,
        nudge_x = 0.5,
        size = 4,
        color = "black"
      ) +
      labs(y = strGroupLabel, x = "Reason Count", fill = "Reason", title = paste0(strGroupLabel, " by Discontinuation Reason")) +
      theme_classic(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank()
      )
  } else {
    distinct_n_y <- df_counts %>% dplyr::distinct(.data[[compreas_name]]) %>% nrow()

    group_reasons_bar <- df_counts %>%
      ggplot(
        aes(
          x = n,
          y = .data[[compreas_name]],
          fill = .data[[var_name]],
          text = paste0(
            strGroupLabel, ": ", .data[[var_name]],
            "\nDiscontinuation Reason: ", .data[[compreas_name]],
            "\nCount: ", n
          )
        )
      ) +
      geom_col() +
      geom_text(
        data = df_counts %>% dplyr::group_by(.data[[compreas_name]]) %>% dplyr::summarise(n = sum(n), .groups = "drop"),
        aes(x = n, y = .data[[compreas_name]], label = n),
        inherit.aes = FALSE,
        nudge_x = 0.5,
        size = 4,
        color = "black"
      ) +
      labs(y = "Reason", x = "Reason Count", fill = strGroupLabel, title = paste0("Discontinuation Reason by ", strGroupLabel)) +
      theme_classic(base_size = 11) +
      theme(
        axis.text.y = element_text(angle = 45, vjust = 1),
        panel.grid.major.y = element_blank()
      )
  }

  # Create plotly
  plotly::ggplotly(group_reasons_bar, tooltip = c("text"), h = calc_fig_size(n_rows = distinct_n_y)) %>%
    layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
}
