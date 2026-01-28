# Bar Chart by Group and Reasons

Bar Chart by Group and Reasons

## Usage

``` r
reasons_groupBar(df, varGroupID, varCompreas, strGroupLabel)
```

## Arguments

- df:

  A `data.frame` containing the participant level dataset with
  eligibility

- varGroupID:

  A variable to make the stacked bar chart with, i.e. invid.

- varCompreas:

  A variable to identify study completion/discontinuation reasons

- strGroupLabel:

  A `string` to label the `varGroupID` in reference to axes, legend,
  footnotes.

## Value

A `plotly` object
