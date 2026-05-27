# Handle immersion data from Lotek logger

This function processes immersion data from a Lotek logger file. It
reads the file, extracts date-time and conductivity information,
performs error checking for data quality, and returns a cleaned data
frame with standardized conductivity values.

## Usage

``` r
handle_immersion_lotek(filepath)
```

## Arguments

- filepath:

  The full path to the Lotek logger file to be processed.

## Value

A cleaned data frame with date-time, conductivity, and standardized
conductivity information, or NULL if the file fails quality checks.
