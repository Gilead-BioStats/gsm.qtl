#' Report_QTL function
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function generates a QTL report based on the provided inputs.
#'
#' @param strOutputDir The output directory path for the generated report. If not provided,
#'  the report will be saved in the current working directory.
#' @param strOutputFile The output file name for the generated report. If not provided,
#'  the report will be named based on the study ID, Group Level and Date.
#' @param dfResults A results `data.frame` from the output of `gsm.reporting::BindResults()` used
#' to create a variety of visualizations like the line plot, bar plot.
#' @param dfGroups A groups `data.frame` from the output of the `Groups.yaml` of `gsm.reporting`used
#' to create a variety of visualizations like the line plot, bar plot.
#' @param lListings A `list` of `data.frame`s that are used as listings to represent participants that are the numerators
#' of the visualizations from `dfResults`.
#'
#' @return File path of the saved report html is returned invisibly. Save to object to view absolute output path.
#' @export
#'

Report_QTL <- function(
    dfResults = NULL,
    dfGroups = NULL,
    lListings = NULL,
    strOutputDir = getwd(),
    strOutputFile = NULL,
    strInputPath =  system.file("report", "Report_QTL2.Rmd", package = "gsm.qtl")
) {
  rlang::check_installed("rmarkdown", reason = "to run `Report_QTL()`")
  rlang::check_installed("knitr", reason = "to run `Report_QTL()`")

  rmarkdown::render(
    input =  strInputPath,
    output_file = file.path(strOutputDir, strOutputFile),
    params =  list(
      dfResults = dfResults,
      dfGroups = dfGroups,
      lListings = lListings
    ),
    intermediates_dir = tempdir(),
    envir = new.env(parent = globalenv())
  )
}
