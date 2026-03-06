# Stacked Discontinuation Bar Chart

Stacked Discontinuation Bar Chart

## Usage

``` r
discontinuation_groupBar(
  df,
  varGroupID,
  strGroupLabel,
  varStatus = compyn,
  valuesDiscontinued = c("N")
)
```

## Arguments

- df:

  A `data.frame` containing the participant level dataset with
  discontinuation

- varGroupID:

  A variable to make the stacked bar chart with, i.e. invid

- strGroupLabel:

  A `string` to label the `varGroupID` in reference to axes, legend,
  footnotes.

- varStatus:

  A variable indicating participant study status, defaults to `compyn`.

- valuesDiscontinued:

  A vector of values in `varStatus` considered discontinued, defaults to
  `c("N")`.

## Value

A `plotly` object
