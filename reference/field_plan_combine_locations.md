# Combine duplicate locations/species in the field plan sheet

This function aggregates locations with the same colony name, species
and age together by summing all numeric variables.

## Usage

``` r
field_plan_combine_locations(field_plan_sheet)
```

## Arguments

- field_plan_sheet:

  A data frame containing the field plan data

## Value

A data frame with combined locations/species

## Examples

``` r
if (FALSE) { # \dontrun{
field_plan_combine_locations(field_plan_sheet)
} # }
```
