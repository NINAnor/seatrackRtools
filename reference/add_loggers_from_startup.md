# Attempt to add logger from startup sheets

This function attempts to add a logger to the master startup data frame
from the startup sheets. Because the data quality of older startup
sheets is variable, the function checks for column mismatches and skips
these files. Incorrectly formatted datetime columns can also lead to
issues.

## Usage

``` r
add_loggers_from_startup(master_startup, partner_metadata)
```

## Arguments

- master_startup:

  A data frame containing the master startup and shutdown information.

- partner_metadata:

  Dataframe of loggers handled by partners

## Value

A new version of the master startup data frame, with the logger added if
succesful.

## Examples

``` r
if (FALSE) { # \dontrun{
updated_master_startup <- add_loggers_from_startup_sheets(master_startup)
} # }
```
