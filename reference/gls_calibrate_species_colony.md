# Prepare a seatrack species/colony combination for calibration

Uses hard coded file paths to call gls_prepare_calibration. Filters
metadata passed to gls_prepare_calibration by species/colony.

## Usage

``` r
gls_calibrate_species_colony(
  species = NULL,
  colony = NULL,
  import_directory = file.path(the$sea_track_folder,
    "Database\\Imports_Logger data\\Raw logger data\\ALL"),
  no_pos_only = TRUE,
  rerun_existing = TRUE,
  include_existing = TRUE,
  filter_plots = FALSE,
  rerun_existing_plots = TRUE,
  new_filter_settings = FALSE
)
```

## Arguments

- species:

  Species name to filter metadata.

- colony:

  Colony name to filter metadata.

- import_directory:

  Path to the directory containing GLS files.

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

- rerun_existing_plots:

  Logical indicating whether to rerun calibration for loggers that
  already have calibration plots. Default is TRUE.

- new_filter_settings:

  Logical indicating whether to force creation of a new filter settings
  file for the species/colony combination, using the seatrackRgls
  defaults. Default is FALSE. If TRUE, existing filter settings file
  will be overwritten.

## Value

None. The function saves the prepared calibration data to the specified
output directory.
