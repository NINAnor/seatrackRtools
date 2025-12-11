# Handle existing GLS calibration data

Function to reshape existing seatrack GLS calibration settings for
immediate use in seatrackRgls

## Usage

``` r
gls_seatrack_calibration(metadata_path, split_years = "06-01")
```

## Arguments

- metadata_path:

  Path to metadata Excel file

- split_years:

  Character string indicating the month and day to split years for
  calibration (e.g., "06-01" for June 1st). Default is "06-01".

## Value

A list with two dataframes: calibration_data and extra_metadata
