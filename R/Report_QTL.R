#' Report_QTL function
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function generates a QTL report based on the provided inputs.
#' @param strInputPath Path to template of the report
#' @param strOutputDir The output directory path for the generated report. If not provided,
#'  the report will be saved in the current working directory.
#' @param strOutputFile The output file name for the generated report. If not provided,
#'  the report will be named based on the study ID, Group Level and Date.
#' @param dfResults A results `data.frame` from the output of `gsm.reporting::BindResults()` used
#' to create a variety of visualizations like the line plot, bar plot.
#' @param dfMetrics A results `data.frame` from the output of `gsm.reporting::MakeMetric()`
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
  dfMetrics = NULL,
  dfGroups = NULL,
  lListings = NULL,
  strOutputDir = getwd(),
  strOutputFile = NULL,
  strInputPath = system.file("report", "Report_QTL.Rmd", package = "gsm.qtl")
) {
  gsm.core::stop_if(cnd = !is.data.frame(dfResults), message = "dfResults must be a data.frame")
  gsm.core::stop_if(cnd = !is.data.frame(dfMetrics), message = "dfMetrics must be a data.frame")
  gsm.core::stop_if(cnd = !is.data.frame(dfGroups), message = "dfGroups must be a data.frame")
  gsm.core::stop_if(cnd = !(is.list(lListings) && !is.data.frame(lListings)), message = "lListings must be a list")

  required_results_cols <- c("MetricID", "GroupID", "GroupLevel", "Numerator", "Denominator", "Metric", "SnapshotDate")
  missing_results_cols <- setdiff(required_results_cols, names(dfResults))
  gsm.core::stop_if(
    cnd = length(missing_results_cols) > 0,
    message = paste0("dfResults is missing required column(s): ", paste(missing_results_cols, collapse = ", "))
  )

  required_metrics_cols <- c("MetricID", "nPropRate", "nNumDeviations")
  missing_metrics_cols <- setdiff(required_metrics_cols, names(dfMetrics))
  gsm.core::stop_if(
    cnd = length(missing_metrics_cols) > 0,
    message = paste0("dfMetrics is missing required column(s): ", paste(missing_metrics_cols, collapse = ", "))
  )

  gsm.core::stop_if(
    cnd = length(lListings) == 0,
    message = "lListings must be a non-empty list"
  )
  invalid_listings <- !vapply(
    lListings,
    function(x) is.data.frame(x) && "studyid" %in% names(x),
    logical(1)
  )
  if (any(invalid_listings)) {
    first_bad <- which(invalid_listings)[1]
    gsm.core::stop_if(
      cnd = TRUE,
      message = paste0("lListings[[", first_bad, "]] must be a data.frame containing studyid")
    )
  }

  gsm.core::stop_if(
    cnd = !(is.character(strOutputDir) && length(strOutputDir) == 1 && nzchar(strOutputDir)),
    message = "strOutputDir must be a single non-empty character value"
  )
  if (!dir.exists(strOutputDir)) {
    dir.create(strOutputDir, recursive = TRUE, showWarnings = FALSE)
  }
  gsm.core::stop_if(cnd = !dir.exists(strOutputDir), message = "strOutputDir does not exist and could not be created")

  gsm.core::stop_if(
    cnd = !(is.character(strOutputFile) && length(strOutputFile) == 1 && nzchar(strOutputFile)),
    message = "strOutputFile must be a single non-empty character value"
  )

  gsm.core::stop_if(
    cnd = !(is.character(strInputPath) && length(strInputPath) == 1 && nzchar(strInputPath) && file.exists(strInputPath)),
    message = "strInputPath must be a single existing file path"
  )

  rlang::check_installed("rmarkdown", reason = "to run `Report_QTL()`")
  rlang::check_installed("knitr", reason = "to run `Report_QTL()`")

  rmarkdown::render(
    input = strInputPath,
    output_file = file.path(strOutputDir, strOutputFile),
    params = list(
      dfResults = dfResults,
      dfMetrics = dfMetrics,
      dfGroups = dfGroups,
      lListings = lListings
    ),
    intermediates_dir = tempdir(),
    envir = new.env(parent = globalenv())
  )
}
