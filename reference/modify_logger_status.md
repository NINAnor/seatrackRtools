# Modify logger status

Function to modify a logger status, such as download_date and
shutdown_date if the logger session is open.

## Usage

``` r
modify_logger_status(
  logger_id,
  new_data = list(),
  master_sheet = NULL,
  all_master_sheet = NULL,
  nonresponsive_list = list()
)
```

## Arguments

- logger_id:

  logger serial number

- new_data:

  named list of new data to be inserted, where the name of each element
  corresponds to a column in the sheet.

- master_sheet:

  master sheet in which the logger will be found. If not provided, all
  sheets will be checked.

- all_master_sheet:

  If master_sheet is not provided, all sheets will be checked. To save
  these being loaded every time, they can be provided using
  `load_all_master_import(combine = FALSE)`

- nonresponsive_list:

  A named list of tibbles, each containing nonresponsive logger data for
  different manufacturers.

## Value

list containing modified version of master_sheet, modified version of
nonresponsive_list
