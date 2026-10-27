# Load a field plan sheet

This function loads a field plan sheet from the specified path and year.
If `return_df` is set to TRUE, it returns a data frame containing the
processed field plan data; otherwise, it returns the workbook object.

## Usage

``` r
load_field_plan_sheet(
  field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
  field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
  return_df = TRUE
)
```

## Arguments

- field_plan_path:

  The file path to the field plan Excel sheet. Default is
  "Admin/Fieldplanning" within the SEATRACK folder.

- field_year:

  The year for which to load the field plan (default is the current
  year)

- return_df:

  A logical value indicating whether to return a data frame (TRUE) or
  the workbook object (FALSE). Default is TRUE.

## Value

A data frame containing the processed field plan data
