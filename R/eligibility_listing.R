#' Generate Eligibility Listing
#'
#' @param df A `data.frame` containing the participant level dataset with eligibility
#' @param download a logical to determine whether or not to return a gt object or a plain data.frame ready for download
#' @returns A `gt` object or `data.frame`
#' @export
eligibility_listing <- function(df, download = FALSE) {
  listing <- df %>%
    filter(Source != "Neither") %>%
    select(invid, country, subjid, Source, ietestcd_concat, dvdtm, eligibility_criteria) %>%
    tidyr::separate(
      dvdtm,
      into = paste0("PD Date", seq_len(max(sapply(strsplit(df$dvdtm, ";;;"), length)))),
      sep = ";;;",
      fill = "right"
    ) %>%
    mutate_at(vars(starts_with("PD Date")), ~ substr(.x, 1, 10)) %>%
    tidyr::separate(
      eligibility_criteria,
      into = paste0("PD Term", seq_len(max(sapply(strsplit(df$eligibility_criteria, ";;;"), length)))),
      sep = ";;;",
      fill = "right"
    ) %>%
    rename(
      Site = invid,
      Country = country,
      `Subject ID` = subjid,
      `Which I/E` = ietestcd_concat
    )

  if(download == TRUE) {
    return(listing)
  }
  else {
    listing %>%
      gt::gt() %>%
      text_transform(
        locations = cells_body(columns = c(`Which I/E`, starts_with("PD Term"))),
        fn = function(x) {
          vapply(x, function(txt) {
            if (is.na(txt) || !nzchar(txt)) {
              return("")
            }

            # Tooltip: escape to be safe
            tooltip <- htmlEscape(txt)

            # Visual truncation logic based on plain text
            plain <- gsub("<[^>]+>", "", txt)
            should_truncate <- nchar(plain) > 10

            display_html <- if (should_truncate) {
              # You could optionally also truncate the HTML, but that's tricky.
              txt # just use full HTML; visual cutoff will be via CSS
            } else {
              txt
            }

            html(
              sprintf(
                '<div title="%s" style="white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100px;">%s</div>',
                tooltip, display_html
              )
            )
          }, FUN.VALUE = html(""))
        }
      ) %>%
      scrollable_gt(., height = "500px")
  }
}
#' Add scrollability to listings
#'
#' @param gt_tbl a `gt` object
#' @param height a string to denote pixel height
#' @param min_table_width a string to denote pixel width
#'
#' @returns A `gt` object
#' @export
scrollable_gt <- function(gt_tbl, height = "300px", min_table_width = "1200px") {
  div(
    style = sprintf("
      max-height: %s;
      overflow-y: auto;
      overflow-x: auto;
      border: 1px solid #ccc;
    ", height),
    div(
      style = sprintf("min-width: %s;", min_table_width),
      gt_tbl
    )
  )
}
