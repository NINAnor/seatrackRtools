# Modify logger status to end a session

Function to modify a logger status, setting download_date, shutdown_date
and logger status

## Usage

``` r
end_logger_session(
  logger_id,
  logger_status,
  downloaded_by = "",
  download_date = NULL,
  comment = "",
  master_sheet = NULL,
  all_master_sheet = NULL,
  nonresponsive_list = list()
)
```

## Arguments

- logger_id:

  logger serial number

- logger_status:

  Character string giving the new logger `download_type`

- downloaded_by:

  Character string containing the name of the user who is downloading
  the data. If NULL this will not be modified

- download_date:

  Date to set the download_date/shutdown_date to. If NULL, this will be
  today's date.

- comment:

  Optional comment to append to the existing comment string.

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
