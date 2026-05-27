# Handle light data from Lotek logger

This function processes light data from a Lotek logger file. It reads
the file, extracts date-time and light information, performs error
checking for data quality, and returns a cleaned data frame with
standardized light values.

## Usage

``` r
handle_light_lotek(filepath)
```

## Arguments

- filepath:

  The full path to the Lotek logger file to be processed.

## Value

A cleaned data frame with date-time, light, and standardized light
information, or NULL if the file fails quality checks.
