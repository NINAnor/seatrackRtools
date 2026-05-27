# Set master import paths

This function allows setting the master import paths directly, for
example if they have been discovered through other means. This will
overwrite any previously stored paths.

## Usage

``` r
set_master_import_paths(location_paths = NULL)
```

## Arguments

- location_paths:

  A named list of paths, where the names are colony names and the values
  are file paths to the master import sheets for those colonies.
