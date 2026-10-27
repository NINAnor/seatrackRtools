# Read and process a field plan Excel sheet

This function reads a field plan Excel sheet and processes the data to
ensure it is in the correct format. It performs a few basic checks and
transformations, including converting certain columns to numeric,
removing strings in brackets from colony names, combining duplicate
locations/species, and checking locations against the SEATRACK database.
A database connection is required to check the locations against the
SEATRACK database.

## Usage

``` r
get_field_plan_df(field_plan_wb = NULL)
```

## Arguments

- field_plan_wb:

  The workbook object of the field plan Excel sheet

## Value

A data frame containing the processed field plan data

## Examples

``` r
if (FALSE) { # \dontrun{
get_field_plan_df("path/to/fieldplan.xlsx")
} # }
```
