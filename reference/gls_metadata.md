# Get metadata from database based on GLS files in import directory

Function to scan a directory for GLS files and retrieve corresponding
metadata from the Sea Track database.

## Usage

``` r
gls_metadata(import_directory, no_pos_only = TRUE)
```

## Arguments

- import_directory:

  Path to the directory containing GLS files.

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

## Value

A dataframe containing metadata for the GLS loggers found in the import
directory.
