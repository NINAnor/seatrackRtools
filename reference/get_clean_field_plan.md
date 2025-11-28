# Read and clean the field plan data

This function processes the field plan data by selecting relevant
columns, renaming them, calculating deployment and retrieval success
rates, and merging with historical data to compute previous deployments.
A connection to the SEATRACK database is required to calculate retrieval
success.

## Usage

``` r
get_clean_field_plan(field_plan_sheet)
```

## Arguments

- field_plan_sheet:

  A data frame containing the raw field plan data

## Value

A cleaned data frame with deployment and retrieval statistics

## Examples

``` r
if (FALSE) { # \dontrun{
field_plan_sheet <- get_field_plan("path/to/fieldplan.xlsx")
seatrackR::connectSeatrack()
field_plan_clean <- get_clean_field_plan(field_plan_sheet)
} # }
```
