# Prepare a seatrack species/colony combination for calibration

Uses hard coded file paths to call gls_prepare_calibration. Filters
metadata passed to gls_prepare_calibration by species/colony.

## Usage

``` r
gls_calibrate_species_colony(
  import_directory = file.path(the$sea_track_folder,
    "Database\\Imports_Logger data\\Raw logger data\\ALL"),
  species,
  colony,
  no_pos_only = TRUE,
  rerun_existing = TRUE,
  include_existing = TRUE,
  filter_plots = FALSE
)
```

## Arguments

- import_directory:

  Path to the directory containing GLS files.

- species:

  Species name to filter metadata.

- colony:

  Colony name to filter metadata.

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

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
