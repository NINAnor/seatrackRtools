# Get metadata from database based on GLS files in import directory

Function to scan a directory for GLS files and retrieve corresponding
metadata from the Sea Track database.

## Usage

``` r
gls_metadata(
  import_directory,
  colony = NULL,
  species = NULL,
  id_year_model = NULL,
  time_windows = TRUE,
  split_years = "06-01",
  no_pos_only = TRUE,
  download_types = c("Successfully downloaded", "Reconstructed")
)
```

## Arguments

- import_directory:

  Path to the directory containing GLS files.

- colony:

  Colony name to filter metadata. Default is NULL (no filtering).

- species:

  Species name to filter metadata. Default is NULL (no filtering).

- id_year_model:

  Character string in the format "loggerID_year_model" to filter for a
  specific logger, year and model. Default is NULL (no filtering).

- time_windows:

  Logical indicating whether to split metadata into time windows based
  on deployment/retrieval dates. Default is TRUE.

- split_years:

  Character string indicating the month and day to split years for
  calibration (e.g., "06-01" for June 1st). Default is "06-01".

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

- download_types:

  Character vector of logger download types to include (e.g.,
  c("Successfully downloaded", "Reconstructed")). Default is
  c("Successfully downloaded", "Reconstructed").

## Value

A dataframe containing metadata for the GLS loggers found in the import
directory.
