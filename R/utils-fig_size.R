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

calc_plotly_footnote_layout <- function(
  footnote_text = NULL,
  margins = list(l = 50, r = 50, b = 50, t = 50),
  wrap_width = 85,
  line_height = 16,
  padding = 18
) {
  if (is.null(footnote_text) || !nzchar(trimws(footnote_text))) {
    return(list(margin = margins, annotations = list()))
  }

  wrapped_text <- strwrap(footnote_text, width = wrap_width)
  n_lines <- max(1L, length(wrapped_text))
  bottom_margin <- margins$b + (n_lines * line_height) + padding

  list(
    margin = utils::modifyList(margins, list(b = bottom_margin)),
    annotations = list(list(
      x = 0,
      y = 0,
      text = paste(wrapped_text, collapse = "<br>"),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "top",
      xshift = 0,
      yshift = -padding,
      align = "left",
      font = list(size = 10)
    ))
  )
}
