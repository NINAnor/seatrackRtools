# Correct logger models in metadata based on database values

This function checks the logger models in the metadata against the
database values and corrects any mismatches. It also uses logger startup
information to fill in missing logger models if available. Database
connection is required

## Usage

``` r
correct_models(metadata, startups = NULL)
```

## Arguments

- metadata:

  Metadata dataframe to correct

- startup:

  Optional dataframe of logger startups to use for correction if
  database values are missing

## Value

Corrected metadata dataframe
