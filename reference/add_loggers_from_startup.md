# Attempt to add logger from startup sheets

This function attempts to add a logger to the master startup data frame
from the startup sheets. Because the data quality of older startup
sheets is variable, the function checks for column mismatches and skips
these files. Incorrectly formatted datetime columns can also lead to
issues.

## Usage

``` r
add_loggers_from_startup(master_import, new_metadata, can_dummy_models = NULL)
```

## Arguments

- master_import:

  Loaded Master startup file.

- new_metadata:

  Loaded filled metadata sheet.

- can_dummy_models:

  Optional data frame of models that can be used to create dummy start
  times. If NULL, the function will retrieve the list of models from the
  database.

## Value

A new version of the master startup data frame, with the logger added if
succesful.

## Examples

``` r
if (FALSE) { # \dontrun{
updated_master_startup <- add_loggers_from_startup_sheets(master_startup)
} # }
```
