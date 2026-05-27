# Get raw data from previous sessions from an excel sheet

This function gets any sessions occuring before the latest session of a
seatrack GLS logger and collects the associated raw data files into a
zip file. The zip file also contains a CSV with metadata about the
previous sessions. This history is useful when a manufacturer is
attempting to recover data from a nonresponsive logger.

## Usage

``` r
get_raw_data_prev_session_excel(
  sheet_path,
  zip_filename = NULL,
  search_path = NULL
)
```

## Arguments

- sheet_path:

  The path to the excel sheet containing logger serial numbers.

- zip_filename:

  The name of the output zip file. Defaults to "raw_data
  \<current_date\>.zip".

- search_path:

  The path to search for raw data files. If NULL, defaults to the "ALL"
  folder in the SeaTrack database imports directory.

## Value

None, but exports a zip file containing the raw data files and a
metadata CSV.
