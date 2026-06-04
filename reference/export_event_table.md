# Annual report event tables This function exports an excel file of an annual report event table. The table is generated using the get_event_table_ft function, which creates a flextable object based on the field plan and database. The export_event_table function then formats the table and saves it as an excel file in the specified directory.

Annual report event tables This function exports an excel file of an
annual report event table. The table is generated using the
get_event_table_ft function, which creates a flextable object based on
the field plan and database. The export_event_table function then
formats the table and saves it as an excel file in the specified
directory.

## Usage

``` r
export_event_table(
  event_table_i,
  field_year,
  current_table_name,
  body_cell_width,
  header_heights,
  export_dir
)
```

## Arguments

- event_table_i:

  A flextable object representing the event table to be exported.

- field_year:

  The year for which the event table is being generated, used in the
  sheet name and file name of the exported excel file.

- current_table_name:

  The name of the current table, used in the sheet name and file name of
  the exported excel file.

- body_cell_width:

  The width of the body cells in the exported excel file.

- header_heights:

  The heights of the header rows in the exported excel file.

- export_dir:

  The directory where the exported excel file will be saved.
