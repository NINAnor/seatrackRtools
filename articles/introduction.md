# Example script

## Setup

Loading the library:

``` r
library(seatrackRtools)
```

There are a few things that need to be set before the functions can be
used.

It is important that we can monitor changes made and the reasoning
behind them. This package uses the
[logger](https://cran.r-project.org/web/packages/logger/index.html)
package for logging. This will print messages to the console and write
them to a file.

``` r
start_logging()
```

By default the logging level is set to **INFO**. You can also set the
scripts to be more verbose by changing the log threshold using commands
from the logger library:

``` r
library(logger)
log_threshold(TRACE)
```

For more information about different logger levels see
[log_levels](https://daroczig.github.io/logger/reference/log_levels.html).

You also need to point the package to where the seatrack directory is
placed in your file system. This variable will be used by many functions
contained in this package.

``` r
path_to_seatrack <- file.path("a_filepath","SEATRACK - shared")
set_sea_track_folder(path_to_seatrack)
```

    #> INFO [2026-01-09 12:00:00] Sea track folder set to: /tmp/Rtmp6N6pxf/seatrack_vignette_2a7a7f0e19c6/SEATRACK - shared

## Loading data

Once this is set up you can open a partner metadata file. Note that the
files shown in this vignette are missing a number of columns present in
the real files.

You can check for non_processed sheets by location, or just enter a path
directly.

``` r
unprocessed_metadata_paths <- get_location_unprocessed(location)
```

The partner metadata excel can then be loaded into R.

``` r

partner_data <- load_partner_metadata(partner_xlsx)
#> Warning in (function (file, sheet, start_row = NULL, start_col = NULL,
#> row_names = FALSE, : variable from `types` not found in data
```

You can then load the master file that you wish to update. This can be
done by using the colony name.

``` r
master_import <- load_master_import("TestColony")
#> Warning in FUN(X[[i]], ...): NAs introduced by coercion
#> Warning in FUN(X[[i]], ...): NAs introduced by coercion
#> Warning in convert_date(x, origin = origin): NAs introduced by coercion
#> Warning in convert_date(x, origin = origin): NAs introduced by coercion
#> Warning in convert_date(x, origin = origin): NAs introduced by coercion
#> Warning in convert_datetime(x, origin = origin): NAs introduced by coercion
```

The function returns a class with two elements, `data` and `wb`. `wb` is
the original excel workbook from which the data was loaded. `data` is a
list, where each element is a sheet from the imported master file.

``` r
names(master_import$data)
#> [1] "METADATA"         "STARTUP_SHUTDOWN"
head(master_import$data$METADATA)
#> # A tibble: 1 × 6
#>   date       ring_number logger_id_deployed logger_id_retrieved colony   comment
#>   <date>     <chr>       <chr>              <chr>               <chr>    <chr>  
#> 1 2024-01-10 42          L1                 #N/A                TestCol… A smal…
```

Optionally, you can also load or initialise some sheets of nonresponsive
loggers. This function takes a vector of file paths and a vector of
manufacturers.

Initialise the sheets if they don’t exist.

``` r
nonresponsive_list <- load_nonresponsive(
  c(lotek_path, migrate_path),
  c("Lotek", "Migrate Technology")
)
```

## Workflow

There are several steps to updating the master import sheet. Firstly
checking startup files for missing logger sessions.

``` r
master_import_data <- master_import$data
updated_startup <- add_loggers_from_startup(master_import_data$`STARTUP_SHUTDOWN`, partner_data$data$`ENCOUNTER DATA`)
```

Then appending reported encounter data.

``` r
updated_metadata <- append_encounter_data(
  master_import_data$METADATA,
  partner_data$data$`ENCOUNTER DATA`
)
#> SUCCESS [2026-01-09 12:00:01] Appended 1 rows to master metadata. New total is 2 rows.
```

Finally the processing reported logger returns and attempts to update
the appropriate session in the master startup sheet, setting download
status and dates.

This function also handles loggers that the partner restearts,
generating new sessions for them in the master startup sheet.

The function will also update the nonresponsive lists.

``` r
updated_sessions <- handle_returned_loggers(
  "TestColony",
  updated_startup,
  partner_data$data$`LOGGER RETURNS`,
  partner_data$data$`RESTART TIMES`,
  nonresponsive_list
)
#> SUCCESS [2026-01-09 12:00:01] Found unfinished session for logger ID: L1 2025-01-10
#> SUCCESS [2026-01-09 12:00:01] Unfinished session:
#>   logger_serial_no starttime_gmt       intended_species intended_location
#> 1 L1               2024-01-01 00:00:00 bird             TestColony       
#> SUCCESS [2026-01-09 12:00:01] Updated 1 sessions.
#> SUCCESS [2026-01-09 12:00:01] Updated sessions:
#>   logger_serial_no starttime_gmt       download_type download_date
#> 1 L1               2024-01-01 00:00:00 Downloaded    2025-01-10
```

At every stage, the log will report the changes that the functions have
made to the master sheet. The log will also report when it is unable to
handle some part of the partner supplied metadata. This can help
identify areas that need manually fixing in either the partner metadata
sheet or the master sheet.

## Combined workflow

This workflow can be run using a single function,
`handle_partner_metadata` that takes the colony name and the imported
excel files as arguments. It returns a list containing the updated
master sheets, and the updated nonresponsive sheets.

``` r
new_sheets <- handle_partner_metadata(
  "TestColony",
  partner_data,
  master_import
)
#> INFO [2026-01-09 12:00:01] Handle partner metadata for TestColony
#> INFO [2026-01-09 12:00:01] Add missing sessions from start up files
#> INFO [2026-01-09 12:00:01] Append encounter data
#> SUCCESS [2026-01-09 12:00:01] Appended 1 rows to master metadata. New total is 2 rows.
#> INFO [2026-01-09 12:00:01] Update sessions from logger returns
#> SUCCESS [2026-01-09 12:00:01] Found unfinished session for logger ID: L1 2025-01-10
#> SUCCESS [2026-01-09 12:00:01] Unfinished session:
#>   logger_serial_no starttime_gmt       intended_species intended_location
#> 1 L1               2024-01-01 00:00:00 bird             TestColony       
#> SUCCESS [2026-01-09 12:00:01] Updated 1 sessions.
#> SUCCESS [2026-01-09 12:00:01] Updated sessions:
#>   logger_serial_no starttime_gmt       download_type download_date
#> 1 L1               2024-01-01 00:00:00 Downloaded    2025-01-10
new_master_sheets <- new_sheets$master_import
new_nonresponsive_list <- new_sheets$nonresponsive_list
```

Finally, if you are happy with the changes made, you can save the
updated master sheet.

``` r
save_master_sheet(new_master_sheets)
```

And the new/modified nonresponsive sheets.

``` r
save_nonresponsive(c(lotek_path, migrate_path), new_nonresponsive_list)
```
