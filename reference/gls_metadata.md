# Get metadata from database based on GLS files in import directory

Function to scan a directory for GLS files and retrieve corresponding
metadata from the Sea Track database.

## Usage

``` r
gls_metadata(
  import_directory,
  colony = NULL,
  species = NULL,
  time_windows = TRUE,
  split_years = "06-01",
  no_pos_only = TRUE
)
```

## Arguments

- import_directory:

  Path to the directory containing GLS files.

- colony:

  Colony name to filter metadata. Default is NULL (no filtering).

- species:

  Species name to filter metadata. Default is NULL (no filtering).

- time_windows:

  Logical indicating whether to split metadata into time windows based
  on deployment/retrieval dates. Default is TRUE.

- split_years:

  Character string indicating the month and day to split years for
  calibration (e.g., "06-01" for June 1st). Default is "06-01".

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

## Value

A dataframe containing metadata for the GLS loggers found in the import
directory.
