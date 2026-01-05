# GLS workflow

## Setup

Loading the library:

``` r
library(seatrackRtools)
library(seatrackRgls)
```

Prepare calibration data. This function will scan `import_directory` for
light files and pull appropriate metadata from the database. If
`no_pos_only` is `TRUE` it will exclude cases where a logging session
already has data in the database. Calibration plots and rows in the
calibration template will not be included for these sessions. It will
also load existing calibration data in the older format from
`existing_calibration_dir`. Calibration plots will not be generated for
these already calibrated. If `include_existing` is TRUE, then these
calibration values will be added to the calibration template.

``` r
existing_calibration_dir <- file.path(the$sea_track_folder, "Locations")
import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")
output_directory <- "test"
no_pos_only <- TRUE

gls_prepare_calibration(import_directory, output_directory, no_pos_only, existing_calibration_dir)
```

Do your calibration. Look at the charts!

Process your data using the calibration values

``` r
calibration_data <- file.path(output_directory, "calibration")
gls_process_positions(
  import_directory,
  calibration_data,
  output_directory
)
```

Prep data for database

``` r
pos_path <- file.path(output_directory, "positions")
all_pos <- gls_prepare_folder_upload(pos_path)
```
