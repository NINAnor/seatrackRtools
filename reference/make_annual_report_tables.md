# Export all tables for annual report

This function generates tables for the annual report based on the field
plan, metadata and database.

## Usage

``` r
make_annual_report_tables(
  field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
  field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
  body_cell_width = 9,
  event_table_header_heights = c(100, 30),
  export_dir = file.path(the$sea_track_folder,
    "Admin/01_Annual reports/Status_Report_for_funders_"),
  all_metadata = NULL,
  new_locations = NULL
)
```

## Arguments

- field_plan_path:

  Path to field plan Excel file

- field_year:

  Year to generate tables for, default is current year

- body_cell_width:

  Width of body cells in the exported Excel file, default is 9

- event_table_header_heights:

  Heights of header rows in the event tables, default is c(100, 30)

- export_dir:

  Directory to export tables to. Default is "Admin/01_Annual
  reports/Status_Report_for_funders\_" in the sea track folder. The
  field year will be appended to the directory name

- all_metadata:

  Optional data frame of all metadata, if not provided will be loaded
  from master import

- new_locations:

  Optional dataframe of new locations to check against field plan
