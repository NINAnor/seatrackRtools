# Check and update locations in the field plan sheet

This function checks the locations in the field plan sheet against the
SEATRACK database table of colonies, updates the sheet with new
locations if provided, calculates the distance of each ocean area from
Trondheim, and orders the ocean areas by latitude and distance from
Trondheim.

## Usage

``` r
field_plan_check_locations(field_plan_sheet, check_colony = TRUE)
```

## Arguments

- field_plan_sheet:

  A data frame containing the field plan data

- check_colony:

  A logical value indicating whether to stop execution if a colony is
  not found in the database (TRUE) or to log a warning and continue
  (FALSE). Default is TRUE.

## Value

A data frame with updated and ordered locations

## Examples

``` r
if (FALSE) { # \dontrun{
seatrackR::connectSeatrack()
field_plan_sheet <- load_field_plan_sheet("path/to/fieldplan.xlsx")
new_locations <- data.frame(colony = c("Nord-Troms", "\u00C5gotnes"), lat = c(70, 60.40))
field_plan_check_locations(field_plan_sheet, new_locations)
} # }
```
