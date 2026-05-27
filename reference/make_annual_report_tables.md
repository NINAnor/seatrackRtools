# Export tables for annual report

This function generates tables for the annual report based on the field
plan, metadata and database.

## Usage

``` r
make_annual_report_tables(
  field_plan_path,
  export_dir,
  all_metadata = NULL,
  new_locations = NULL,
  field_year = c(as.numeric(format(Sys.Date(), "%Y")))
)
```

## Arguments

- field_plan_path:

  Path to field plan Excel file

- export_dir:

  Directory to export tables to

- all_metadata:

  Optional data frame of all metadata, if not provided will be loaded
  from master import

- new_locations:

  Optional dataframe of new locations to check against field plan

- field_year:

  Year to generate tables for, default is current year
