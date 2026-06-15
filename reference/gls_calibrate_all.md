# Calibrate all species/colony combinations found in seatrack logger data import folder

Uses hard coded file paths to call gls_calibrate_species_colony for all
species/colony combinations found in seatrack database for loggers in
import folder.

## Usage

``` r
gls_calibrate_all(
  no_pos_only = TRUE,
  rerun_existing = FALSE,
  include_existing = TRUE,
  filter_plots = FALSE,
  rerun_existing_plots = FALSE,
  n_workers = 4,
  new_filter_settings = FALSE
)
```

## Arguments

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

- rerun_existing:

  Logical indicating whether to rerun calibration for loggers that
  already have calibration data. Default is FALSE.

- include_existing:

  Logical indicating whether to include existing calibration data in the
  final output. Default is TRUE.

- filter_plots:

  Logical indicating whether to export filter plots. Default is FALSE.

- rerun_existing_plots:

  Logical indicating whether to rerun calibration for loggers that
  already have calibration plots. Default is FALSE.

- n_workers:

  Integer specifying the number of worker processes to use for parallel
  processing. Default is 4.

- new_filter_settings:

  Logical indicating whether to force creation of a new filter settings
  files for each species/colony combination, using the seatrackRgls
  defaults. Default is FALSE. If TRUE, existing filter settings files
  will be overwritten.

## Value

None. The function saves the prepared calibration data to the specified
output directory.
