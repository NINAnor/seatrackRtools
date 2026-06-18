# Get and prepare data for the SEATRACK metadata Shiny app.

This function retrieves and processes data from the SEATRACK database to
prepare it for use in the Shiny app that visualizes metadata. It gathers
information on individuals, logger deployments, and positional data, and
then saves this processed data as CSV files within the package for use
in the app.

## Usage

``` r
get_web_metadata_data()
```

## Value

None. This function processes data and saves it as CSV files for use in
the Shiny app, but does not return a value.
