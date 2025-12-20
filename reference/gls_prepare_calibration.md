# Prepare GLS calibration data using seatrack database

Function to prepare GLS calibration data for use with seatrackRgls,
based on GLS files in the import directory and metadata from the Sea
Track database.

## Usage

``` r
gls_prepare_calibration(
  import_directory,
  output_directory,
  species = NULL,
  colony = NULL,
  no_pos_only = TRUE,
  existing_calibration_dir = NULL,
  rerun_existing = TRUE,
  include_existing = TRUE,
  filter_plots = FALSE,
  rerun_existing_plots = FALSE
)
```

## Arguments

- import_directory:

  Path to the directory containing GLS files.

- output_directory:

  Path to the directory where the prepared calibration data will be
  saved.

- species:

  Species name to filter metadata. Default is NULL (no filtering).

- colony:

  Colony name to filter metadata. Default is NULL (no filtering).

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

- existing_calibration_dir:

  Directory containing existing calibration data. Default is NULL. If
  provided, calibration data from this directory will be merged.

- rerun_existing:

  Logical indicating whether to rerun calibration for loggers that
  already have calibration data. Default is TRUE.

- include_existing:

  Logical indicating whether to include existing calibration data in the
  final output. Default is TRUE.

- filter_plots:

  Logical indicating whether to export filter plots. Default is FALSE.

## Value

None. The function saves the prepared calibration data to the specified
output directory.
