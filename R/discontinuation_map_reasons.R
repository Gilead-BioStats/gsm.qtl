#' Filter Discontinuation Reasons in data.frame
#'
#' @param yaml_path A `string` that denotes path to yaml file that contains discontinuation reasons
#'
#' @returns A `data.frame`
#' @export
discontinuation_map_reasons <- function(df, yaml_path = system.file("workflow", "0_other", "disc_reasons.yaml", package = "gsm.qtl")) {
  yaml_data <- yaml::read_yaml(yaml_path)
  reasons <- yaml_data$steps[[1]]$params$reasons
  df %>% filter(compreas %in% reasons)
}
