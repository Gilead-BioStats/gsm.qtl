#' Filter Discontinuation Reasons in data.frame
#'
#' @param df A `data.frame` derived from mapping STUDCOMP
#' @param strReasons A `string` of reasons for discontinuation
#'
#' @returns A `data.frame`
#' @export
discontinuation_map_reasons <- function(df, strReasons) {
  filtered_df <- df %>%
    dplyr::filter(
      compreas %in% strReasons
    )
  return(filtered_df)
}
