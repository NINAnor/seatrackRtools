# Prepare GLS files for calibration

Uses hard coded file paths to call gls_prepare_calibration. Filters
metadata passed to gls_prepare_calibration by file names.

## Usage

``` r
gls_calibrate_file(
  id_year_model,
  no_pos_only = TRUE,
  rerun_existing = TRUE,
  include_existing = TRUE,
  filter_plots = TRUE,
  rerun_existing_plots = TRUE,
  new_filter_settings = FALSE
)
```

## Arguments

- id_year_model:

  Character string in the format "loggerID_year_loggermodel" to filter
  for a specific logger, year and model.

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

## Value

None. The function saves the prepared calibration data to the specified
output directory.
