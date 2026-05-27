# Handle temperature data from Migrate logger

This function processes temperature data from a Migrate logger file. It
reads the file, extracts date-time and temperature information, performs
error checking for data quality, and returns a cleaned data frame with
standardized temperature values.

## Usage

``` r
handle_temperature_migrate(filepath)
```

## Arguments

- filepath:

  The full path to the Migrate logger file to be processed.

## Value

A cleaned data frame with date-time, temperature, and standardized
temperature information, or NULL if the file fails quality checks.
