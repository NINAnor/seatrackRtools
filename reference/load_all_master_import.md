# Load master import file for all colonies

This function attempts to load all master import sheets available in the
seatrack folder.

## Usage

``` r
load_all_master_import(combine = TRUE, skip = c())
```

## Arguments

- combine:

  Boolean determining whether or not to combine the sheets into a single
  dataframe.

- skip:

  character vector of location names to not load.

## Value

If combine is TRUE: A tibble consisting of combined metadata and
startup_shutdown sheets, with an extra column for path appended to each.
Otherwise a list where every element is a LoadedWB object.
