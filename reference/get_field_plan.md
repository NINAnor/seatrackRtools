# Read and process the field plan Excel sheet

This function reads the specified Excel sheet, cleans the data by
removing rows with missing values in key columns, converts certain
columns to numeric, removes bracketed text from colony names, and
combines duplicate locations/species.

## Usage

``` r
get_field_plan(field_plan_path)
```

## Arguments

- field_plan_path:

  The file path to the field plan Excel sheet

## Value

A data frame containing the processed field plan data

## Examples

``` r
if (FALSE) { # \dontrun{
get_field_plan("path/to/fieldplan.xlsx")
} # }
```
