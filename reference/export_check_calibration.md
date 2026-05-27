# Export calibration check results to Excel

Function to export the results of the calibration check to an Excel
file. It uses the `gls_check_calibration` function to get the
calibration status for all species/colony combinations and then formats
the results into a flextable with colored backgrounds based on the
proportion of calibrated entries. The resulting table is saved as an
Excel file in the specified output directory.

## Usage

``` r
export_check_calibration()
```

## Value

None. The function saves the calibration check results to an Excel file
in the specified output directory.
