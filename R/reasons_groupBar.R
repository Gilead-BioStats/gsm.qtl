#' Bar Chart by Group and Reasons
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param varGroupID A variable to make the stacked bar chart with, i.e. invid
#' @param strGroupLabel A `string` to label the `varGroupID` in reference to axes, legend, footnotes.
#'
#' @returns A `plotly` object
#'
#' @export
reasons_groupBar <- function(df, varGroupID, strGroupLabel) {
  var_sym  <- rlang::ensym(varGroupID)
  var_name <- rlang::as_string(var_sym)

  df_counts <-df %>%
    filter(compreas != "") %>%
    dplyr::count(compreas, !!var_sym, name = "n")

  distinct_n_compreas <- df_counts %>% filter(compreas != "") %>% dplyr::distinct(compreas) %>% nrow()

  # Create GG object
  group_reasons_bar <- df_counts %>%
    # filter(compyn == "N") %>%
    ggplot(aes(x = n, y = compreas, fill = .data[[var_name]])) +
    geom_col(aes(
      text = paste0(
        strGroupLabel, ": ", !!enexpr(varGroupID),
        "\n Discontinuation Reason: ", compreas,
        "\nCount: ", n
      )
    )) +
    labs(y = "Reason", x = "Reason Count", fill = strGroupLabel, title = paste0("Discontinuation Reason by ", strGroupLabel)) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.y = element_text(angle = 45, vjust = 1), # tilt to avoid overlap
      panel.grid.major.y = element_blank()
    )
  # Create plotly
  plotly::ggplotly(group_reasons_bar, tooltip = c("text"), h = calc_fig_size(n_rows = distinct_n_compreas))
    # layout(
    #   margin = list(l = 180, r = 50, b = 60, t = 60),
    #   annotations = list(
    #     x = 1, y = -0.12,
    #     xref = "paper", yref = "paper", showarrow = F,
    #     xanchor = "right", yanchor = "top", xshift = 0, yshift = 0,
    #     font = list(size = 10)
    #   )
    # )
}
