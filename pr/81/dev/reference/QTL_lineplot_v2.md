# QTL Time Series Widget (v2)

QTL Time Series Widget (v2)

## Usage

``` r
QTL_lineplot_v2(dfResults, strQTL)
```

## Arguments

- dfResults:

  A results `data.frame` from the output of
  [`gsm.reporting::BindResults()`](https://gilead-biostats.github.io/gsm.reporting/reference/BindResults.html)
  used to create time-series visualizations.

- strQTL:

  A `string` to label the QTL being measured.

## Value

A `htmlwidget` object.
