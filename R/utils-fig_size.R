#' Calculate figure dimensions based on the data going in
#'
#' @param n_rows number of rows/groups (sites, reasons, etc.)
#' @param base size for each row/group
#' @param per multiplier
#'
#' @returns  an approrpiate number for the size of a figure dimension
#' @export
calc_fig_size <- function(n_rows, base = 500, per = 22) {
  return(max(c(base, (per * n_rows))))  # pixels
}
