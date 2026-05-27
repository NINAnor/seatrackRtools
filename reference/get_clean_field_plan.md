# Read and clean the field plan data

This function processes the field plan data by selecting relevant
columns, renaming them, calculating deployment and retrieval success
rates, and merging with historical data to compute previous deployments.
A connection to the SEATRACK database is required.

## Usage

``` r
get_clean_field_plan(
  field_plan_sheet,
  use_master_sheets = FALSE,
  all_locations = NULL,
  use_db = TRUE,
  field_year = c(as.numeric(format(Sys.Date(), "%Y")))
)
```

## Arguments

- field_plan_sheet:

  A data frame containing the raw field plan data

- use_master_sheets:

  A boolean indicating whether to use master sheets to calculate
  success.

- use_db:

  A boolean indicating whether to use the database to calculate success.

- field_year:

  The year to filter the metadata for when using master sheets (default
  is the current year)

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
