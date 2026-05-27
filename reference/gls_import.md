# Import GLS position data to the SEATRACK database

Function to import prepared GLS position data from a specified directory
to the SEATRACK database.

## Usage

``` r
gls_import(
  species = NULL,
  colony = NULL,
  chunk_size = 10,
  gls_directory_path = file.path(the$sea_track_folder,
    "Database\\Imports_Logger data\\Output_GLSpositions")
)
```

## Arguments

- species:

  A character vector of species names to filter the import. If NULL, all
  species directories in the specified GLS directory will be processed.
  Default is NULL.

- colony:

  A character vector of colony names to filter the import. If NULL, all
  colony directories within the specified species directories will be
  processed. Default is NULL.

- chunk_size:

  Integer specifying the number of sessions to upload in each chunk.
  Default is 10.

- gls_directory_path:

  Path to the directory containing the prepared GLS position data files.
  Default is `Database\\Imports_Logger data\\Output_GLSpositions` within
  the SEATRACK folder.

## Value

None. The function uploads the position data to the SEATRACK database.
