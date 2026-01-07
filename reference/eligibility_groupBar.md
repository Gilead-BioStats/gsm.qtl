# Stacked Eligibility Bar Chart

Stacked Eligibility Bar Chart

## Usage

``` r
eligibility_groupBar(df, varGroupID, strGroupLabel, bPercentage = FALSE)
```

## Arguments

- df:

  A `data.frame` containing the participant level dataset with
  eligibility

- varGroupID:

  A variable to make the stacked bar chart with, i.e. invid

- strGroupLabel:

  A `string` to label the `varGroupID` in reference to axes, legend,
  footnotes.

- bPercentage:

  A `boolean` to denote whether or not the group bar chart should be
  visualized as percentages instead of absolute counts.

## Value

A `plotly` object
