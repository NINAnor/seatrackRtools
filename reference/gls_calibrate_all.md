# Calibrate all species/colony combinations found in seatrack logger data import folder

Uses hard coded file paths to call gls_calibrate_species_colony for all
species/colony combinations found in seatrack database for loggers in
import folder.

## Usage

``` r
gls_calibrate_all(
  no_pos_only = TRUE,
  rerun_existing = TRUE,
  include_existing = TRUE,
  filter_plots = FALSE
)
```

## Arguments

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
