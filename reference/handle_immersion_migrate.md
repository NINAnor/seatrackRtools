# Handle immersion data from Migrate logger

This function processes immersion data from a Migrate logger file. It
reads the file, extracts date-time and conductivity information,
performs error checking for data quality, and returns a cleaned data
frame with standardized conductivity values.

## Usage

``` r
handle_immersion_migrate(filepath)
```

## Arguments

- filepath:

  The full path to the Migrate logger file to be processed.

## Value

A cleaned data frame with date-time, conductivity, and standardized
conductivity information, or NULL if the file fails quality checks.
