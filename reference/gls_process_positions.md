# Process GLS position data using seatrackRgls

Function to process GLS position data from a specified directory using
seatrackRgls. The only additional functionality is fetching colony
information from the Sea Track database.

## Usage

``` r
gls_process_positions(import_directory, calibration_data, output_directory)
```

## Arguments

- import_directory:

  Path to the directory containing GLS files.

- calibration_data:

  Dataframe containing GLS calibration data.

- output_directory:

  Path to the directory where the processed position data will be saved.

## Value

None. The function saves the processed position data to the specified
output directory.
