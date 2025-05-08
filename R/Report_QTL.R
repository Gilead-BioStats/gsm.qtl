#' Report_QTL function
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function generates a KRI report based on the provided inputs.
#'
#' @param lCharts A list of charts to include in the report.
#' @param strOutputDir The output directory path for the generated report. If not provided,
#'  the report will be saved in the current working directory.
#' @param strOutputFile The output file name for the generated report. If not provided,
#'  the report will be named based on the study ID, Group Level and Date.
#' @param dfResults a
#' @param dfGroups b
#'
#' @return File path of the saved report html is returned invisibly. Save to object to view absolute output path.
#' @export
#'

Report_QTL <- function(
    lCharts = NULL,
    dfResults = NULL,
    dfGroups = NULL,
    strOutputDir = getwd(),
    strOutputFile = NULL
) {
  rlang::check_installed("rmarkdown", reason = "to run `Report_QTL()`")
  rlang::check_installed("knitr", reason = "to run `Report_QTL()`")

  RenderRmd(
    strInputPath = system.file("report", "Report_QTL0001.Rmd", package = "gsm.qtl"),
    strOutputFile = strOutputFile,
    strOutputDir = strOutputDir,
    lParams = list(
      lCharts = lCharts,
      dfResults = dfResults,
      dfGroups = dfGroups
    )
  )
}
