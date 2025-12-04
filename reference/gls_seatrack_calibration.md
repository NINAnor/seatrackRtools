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

## Value

A list with two dataframes: calibration_data and extra_metadata
