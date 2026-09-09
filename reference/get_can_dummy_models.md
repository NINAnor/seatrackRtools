# Get logger models that can have dummy start times

This function retrieves logger models that are eligible for creating
dummy start times. It filters out specific producers and logger types
that are not suitable for dummy start times.

## Usage

``` r
get_can_dummy_models()
```

## Value

A data frame containing logger models that can have dummy start times.
