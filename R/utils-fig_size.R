#' Compute a widget height from the number of y-axis groups
#'
#' Convenience helper for HTML widgets (e.g., Plotly) that converts a count of
#' categories/rows into a pixel height. The function returns the larger of
#' `base` and `per * n_rows`, which keeps small plots readable while allowing
#' tall plots when many groups are shown.
#'
#' @param n_rows A single non-negative number giving the number of rows/groups
#'   (e.g., distinct y-axis categories).
#' @param base Minimum height in **pixels** to allocate regardless of `n_rows`.
#'   Default is `500`.
#' @param per Height in **pixels** to allocate **per** row/group. Default is `22`.
#'
#' @return An integer pixel value suitable for a widget's `height =` argument.
#' @export
calc_fig_size <- function(n_rows, base = 500, per = 25) {
  if (!is.numeric(n_rows) || length(n_rows) != 1L || is.na(n_rows) || n_rows < 0) {
    stop("`n_rows` must be a single non-negative number.")
  }
  as.integer(max(base, per * n_rows))
}
