# Get the path to the field plan Excel sheet

This function retrieves the full path to the field plan Excel sheet for
a specified year. It searches for the file in the given directory and
returns the first match found. If no matching file is found, it raises
an error.

## Usage

``` r
get_field_plan_path(
  field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
  field_year = c(as.numeric(format(Sys.Date(), "%Y")))
)
```

## Arguments

- field_plan_path:

  The directory path where the field plan Excel sheet is located
  (default is "Admin/Fieldplanning" within the SEATRACK folder)

- field_year:

  The year for which to retrieve the field plan (default is the current
  year)

## Value

The full path to the field plan Excel sheet
