# Stacked Discontinuation Bar Chart

Stacked Discontinuation Bar Chart

## Usage

``` r
discontinuation_groupBar(
  df,
  varGroupID,
  strGroupLabel,
  varStatus = compreas,
  valuesDiscontinued = c("WITHDRAWAL BY SUBJECT", "NON-COMPLIANCE WITH STUDY DRUG",
    "PROTOCOL VIOLATION", "PHYSICIAN DECISION", "WITHDREW CONESENT", "DEATH",
    "LOST TO FOLLOW UP", "Withdrew Consent", "Death", "Lost to Follow-Up")
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

  A variable indicating participant study status, defaults to
  `compreas`.

- valuesDiscontinued:

  A vector of values in `varStatus` considered premature
  discontinuations, defaults to a string of known reasons.

## Value

A `plotly` object
