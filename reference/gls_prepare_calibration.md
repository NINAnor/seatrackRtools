# Prepare GLS calibration data using seatrack database

Function to prepare GLS calibration data for use with seatrackRgls,
based on GLS files in the import directory and metadata from the Sea
Track database.

## Usage

``` r
gls_prepare_calibration(import_directory, output_directory, no_pos_only = TRUE)
```

## Arguments

- import_directory:

  Path to the directory containing GLS files.

- output_directory:

  Path to the directory where the prepared calibration data will be
  saved.

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

## Value

None. The function saves the prepared calibration data to the specified
output directory.
