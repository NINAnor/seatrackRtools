# Check calibration status for all species/colony combinations

Function to check the calibration status for all species/colony
combinations in the Sea Track database. It looks for existing
calibration files and summarizes the number of calibrated, uncalibrated,
and valid entries for each combination.

## Usage

``` r
gls_check_calibration()
```

## Value

A dataframe summarizing the calibration status for each species/colony
combination, including the number of calibrated, uncalibrated, and valid
entries, as well as the total number of entries and the path to the
calibration file.
