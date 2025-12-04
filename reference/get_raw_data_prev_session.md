# Get raw data from previous sessions

This function gets any sessions occuring before the latest session of a
seatrack GLS logger and collects the associated raw data files into a
zip file. The zip file also contains a CSV with metadata about the
previous sessions.

## Usage

``` r
get_raw_data_prev_session(
  logger_serial_nos = c(),
  zip_filename = paste0("raw_data_", Sys.Date(), ".zip"),
  search_path = NULL
)
```

## Arguments

- logger_serial_nos:

  A character vector of logger serial numbers to get previous session
  data for.

- zip_filename:

  The name of the output zip file. Defaults to
  "raw_data\_\<current_date\>.zip".

- search_path:

  The path to search for raw data files. If NULL, defaults to the "ALL"
  folder in the SeaTrack database imports directory.

## Value

None, but exports a zip file containing the raw data files and a
metadata CSV.

## Details

This history is useful when a manufacturer is attempting to recover data
from a nonresponsive logger.
